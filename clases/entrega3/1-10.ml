(* 1-10 Selección de Instrucciones  *)

Seleccion de instrucciones por maximal munch.
Por supuesto, este codigo se da como ejemplo: el codigo de cada compilador dependera de la arquitectuca elegida y de la imaginacion.

Tendremos dos funciones, munchExp y munchStm.

En tigercodegen.sml

structure tigercodegen :> tifercodegen =
	struct
	opentigertree
	opentigerassem
	opentigerframe

En tigerassem (provisto por Appel) tenemos:

structure tigerassem=
	struct
    type reg = string
    type temp = tigertemp.temp
    type label = tigertemp.label
    
    datatype instr = OPER of {assem=string, src=temp list, dst = temp list, jump label list option }
                    | LABEL of {assem = string, lab = label}
                    | MOVE of {assem = string, src = temp, dst = temp}

Luego viene una funcion para intercalar dentro de assem los nombres de src y dst. 
La convencion es, los fuentes se escriben (dentro de assem) con 's0, 's1, ... y los destinos con 'd0,'d1, ...

Volvemos a codegen

fun codegen frame stm = (*se aplica a cada funcion*)
    let val ilist = ref ([]:instr list) (*lista de instrucciones que va a ir mutando*)
        fun emit = ilist := x::(!ilist) (*!ilist es equivalente a *ilist en C y ilist := a es equivalente a *ilist = a en C*)
        fun results gen = let val t tigertemp.newtemp
                            in gen t; t end
        fun munchStm s = 
            case s of
            (SEQ (a,b) ) => (munchStm a; munchStm b)
            |(MOVE (TEMP t1, BINOP(MINUS, TEMP t2, CONST i)) ) => 
                if t1 = tigerframe.sp andalso t2 = tigerframe.sp then (*fp y sp no tienen que aparecer en ningun momento en src ni dst porque no pueden ser elegidos para guardar un estado intermedio*)
                    emit(OPER{assem = "MOV SP, SP-"^Int.toString i^"\n", src = [], dst = [], jump = NONE}
                else 
                    emit(OPER{assem = "MOV 'd0, 's0-"^Int.toString i^"\n", src = [t2], dst = [t1], jump = NONE}
            | MOVE (MEM e1, MEM e2) => (* si no tenemos mem -> mem generamos t<-mem1 seguido de mem2<-t*)
                let val t = tigertemp.newtemp()
                in emit(OPER{assem = "MOV 'd0, MEM['s0]\n", dst=[t], src = [munchExp e2], jump = NONE} ); emit(OPER{assem = "MOV MEM['d0], 's0\n", dst=[munchExp e1], src = [t], jump = NONE} ) 
                end

A la hora de tratar los casos se recomienda tratar el caso general (por ej. el anterior), y luego tomar casos particulares que puedan salir.

Si generamos código intermedio de la forma:
    MOVE (MEM (CONST i), CONST j).
Se captura con 
    | MOVE (MEM .... ) =>
        emit ( OPER {assem = "MOV M["^Int.toString i^",$"^Int.toString j^"\n", src = [], dst = [], jump = NONE})
    | MOVE (TEMP t1, TEMP t2)
        emit ( MOVE {assem = "MOV 'd0, 's0\n", src = t2, dst=t1})
    | MOE (TEMP t, e) = 
        emit (MOVE {assem = "MOV 'd0, 's0\n", src = {munchExp e, dst = t}) 

Con el fin de entender cómo sigue esto, veamos qué viene después. 

Calculo de fata flow. 
Un temporario se dice que se DEFINE en una instrucción si está en el dst de esa instrucción.
Un temporario se USA en una instrucción si está en el src de ella.
Un temporario está VIVO entre un def y un use.
Veamos un ejemplo.


                     usa     def     live
t1 <- 1                     t1          
t2 <- 2                     t2          t1
t3 <- t1 + t2       t1,t2   t3          t2

call _                                  t2
call _                                  t2
call _                                  t2

t1 <- 2                     t1          t2
return t1+t2        t1,t2    


Veamos como se puede calcular el liveness de un temporario.
Dada una instruccion (nodo) tendremos sus predecesores (pred) (*instrucciones que se pueden ejecutar antes de esta*) y sus sucesores (succs).
Dado un nodo n, definimos 
    In[n]: los temporarios que estan vivos al entrar al nodo.
    Out[n]: los temporarios que estan vivos al salir del nodo.

Veamos In[n] = Use[n] U (Out[n] \ Def[n])
       Out[n] = U(s en succs) In[s]
Estas son las ecuaciones que permiten calcular liveness de temporarios. 
Estas ecuaciones son ecuaciones de conjuntos, y no tienen solucion unica. Nos interesa la solucion minima: SCS; la mas chica que este en todas las otras. Resolveremos esto iterando estas ecuaciones hasta alcanzar un punto fijo. 

Resolucion:
for each nodo n:
    in[n] <- vacio ; out[n] <- vacio
repeat 
    for each n;
        in'[n] <-in[n]
        out'[n] <- out[n]      
        in[n] <- use[n] U (out[n] \def[n])
        out[n] <- U(s en succs) in[s]
until in'[n] = in[n] and out'[n = out[n for all n

(*para obligar que los calle saves esten vivos durante la funcion se agrega al principio de la funcion instrucciones donde los destinos sean los callee saves y al final instrucciones donde las fuentes sean los callee saves *)
