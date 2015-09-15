(* Clase 14-9 *)

(* Más código intermedio
La generación para una función es sencilla *)

fun functionDec(body) =
	let val body' = unEx body
	in Nx(MOVE(TEMP rv,body'))

(* Pero se puede mejorar con un parámetro que indique si la función devuelve o no valor. *)

fun functionDec(body,True) =
	Nx(MOVE(TEMP rv, unEx body)
fun functionDec(body,True) = body

(* Donde las cosas se ponen más peludas en el callExp, puesto que hay que tener en cuenta la convención de llamada (que debemos respetar para linkear con el runtime) y el status de escape de los parámetro. En principio dejemos que el escape sea tratado por la función. *)
	
(* Hay muchas convenciones: i386 pasa todo en stack, de derecha a izquierda (C) ó de izquiewrda a derecha (Pascal,MUDULA) ; MIPS pasa los dos primeros MIPS pasa los dos primeros en v0 y v1, x86_64 pasa los primeros cinco en registro, el resto en stack 
Trataremos de hacer algo genérico
Definimos en frame: *)
val argregs = [registros p/ pasaje de args]
(* y en translate, *)
fun callExp(name, params) =
	(* evaluaremos los parámetros de izq a der *)
	let val params' = List.map unEx params
		fun paramtmps 0 = []
		  | paramtmps n = (TEMP newtmp())::paramtmps (n-1)
		val tmpas = paramtmps (length params - lenth argregs) (* ACÁ HAY QUE ARREGLAR EL CASO NEGATIVO *)
		fun carga l [] = l
		  | carga (arg::t) (temp::s) = ( MOVE(temp,arg) )::carga t s
		fun pzip l [] = List.map (fn(x) => (x,NONE)) l
			pzip [] _ = []
			pzip (h::t) (r::s) = (h,SOME r)::pzip t s
		fun empareja = pzip params' (argregs @ tmpas)
		val enstack = List.filter (fn(_,NONE) => True | _ => False) empareja
(* Luego se puede generar las instrucciones p/cargar en stack (ò mandar los temporarios)
Finalmente se genera: *)
	CALL(nombre, listadelosparams)
(* NOTAORAL: Lo malo de sacarse de ensima el problema acá y no pasar todo en temporarios es que no tenemos push, solo mov e incremento del stack pointer que luego debe ser reconocido como push 
  Nota: queé es mejor para pasar argumentos en la pila el estilo i386 o el RISC?
  i386 usa push para stackear valores:
	push val == subl %esp, $tamaño de val ; movl (%esp) val
  RISC (normalmente) no tiene ni push ni pop con lo que deberíamos hacer
	(supondremos v1, v2, ... vn del mismo tamaño)
		push v1           	sub sp, w          sub sp, n*w
		push v2       =>	mov(sp),v1   =>    mov (sp+(n-1)*w), v1
		...					...                mov (sp+(n-2)*w), v2
		push vn								   ...
*)


(* Canonización del código intermedio
El propósito de esta etapa es transformar el árbol de código intermedio en una lista. Una simple recorrida NO se puede hacer, pues las expreciones pueden tener efectos laterales (en particular las ESEQ). Por esto, atacaremos el problema transformando el árbol en otro canónico.
Un árbol es canónico si:
	 - No tiene SEQ ni ESEQ.
	 - El padre de un CALL es ó un EXP o un MOVE(TEMP t, ...)
¿Como eliminar las ESEQ? Una idea es liftearlas, hasta que quede una sola en la raíz, y allí eliminarla.
Usaremos un sistema de reescritura. (Term Reawriting and All That, Baader & Nipkow)
	1) ESEQ(s1,ESEQ(s2,e)) => ESEQ(SEQ(s1,s2),e)
	2) BINOP( OP, ESEQ(s,e1), e2) => ESEQ(s,BINOP(OP,e1,e2))
	3) BINOP(OP, e1, ESEQ(s,e2)) => ESEQ( MOVE(TEMP t, e1), ESEQ(s,BINOP(oper, TEMP t, e2))  t tiene que ser fresco
	4) CJUMP(uper,e1, ESEQ(s,e2), l1, l2) => SEQ( MOVE(TEMP(t,e1), SEQ(s, CJUMP(oper, TEMP t, e2, l1, l2))))
	Podemos prescinidr el temporarios? Sólo si s no modifica e1. En otras palabras si la ejecición de s y la evaluación de e1 conmutan.
	Ej: si e1 es una constante. (no es imprescindible hacer esto). La única ventaja es que no genera tantos temporarios y la enrigestración es menos pesada. Podríamos hacer una función conmute que diga si una sentencia y una expresión conmutan:
*)
	fun conmute (_,CONST _) = True
	  | conmute (_,NAME _) = True
		...
	  | conmute _ = False
(*
	BINOP(oper, e1, ESEQ(s,e2)) => ESEQ(s,BINOP(oper,e1,e2))
	CJUMP(oper,e1,ESEQ(s,e2),l1,l2) => SEQ(s,CJUMP(oper,e1,e2,l1,l2))

Finalmente queda un secuencia de sentencias. Acá nos interesará partir el código resultante en bloques Básicos. Un boloque básico es una secuencia de instrucciones que cumple que:
	- su primera instrucción es un label
	- su última instancia es un JUMP o CJUMP
	- No tiene otro label ni otros saltos
Tiene la característica que si se ejecuta una instrucción se ejecutan todas.
Pueden cambiarse el orden en el que aparecen en el programa 
Pueden detectar código muerto.
	while 1 do (
		break
		2+3 /* esto es codigo muerto */
	)
Los bloques básicos permiten una serie de optimizaciones para aprovechar mejor las caches.
¿Cómo se generan los BB?
	1º regla:
		[... s1, LABEL l, s2, ... ] -> [ [..., s1 , JUMP l] [label l, s2...] ]
	2º regla:
		[... s1, JUMP l, s2, ... ] -> [ [..., s1 , JUMP l] [s2...] ] (idem con CJUMP)
Finalmente los BB se colectan.
Se generarán dos etiquetas en una función, una al principio (begin) y otra al final (end). Llamar a la función es un CALL a begin y terminar la función es un salto al end.
Esto permite hacer test de dominancia. El bloque básico B1 domina a B2 si cualquier camino de begin a B2 pasa por B1.
 Como se puede optimizar?
	int m[10000][100000]
	for(i=0;i<10;i++)
		for(i=0;i<10;i++)
            ... m[i][j]

    for(i=0;i<10;i++) {
        int *pi = m[i]
		for(i=0;i<10;i++)
            ... p[j]
        } 
*)





(* Codigo p/RecordExp *)

fun recordExp fields = (*deben estar ordenados por el numero de secuencia*)	
    Ex(CONST(CALL("_createRecord",(CONST(length fields))::List.map unEx fields)))
ó let val t = newtemp()
  in Ex(ESEQ(MOVE(TEMP t, CALL("_createRecord",...)), TEMP t))  end

(* En el runtime *)
#include <stdarg.h>
long* _createRecord(long args, ...)
{
    long *tmp; int i; va_list va;
    tmp = malloc(args*sizeof(long));
    va = va_start(args);
    for(i=0;i<args;i++)
        tmp[i]=va_arg(long);
    va_end(va);
    return tmp;
}

(* Próxima clase: detalles de reescritura y canonización...*)










