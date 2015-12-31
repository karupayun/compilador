(*completar traduccion de expresiones del AST a tree.stm o tree.exp. En gral, tigertrans no conoce la maquina de destino,
pero si el lenguaje fuente*)

structure tigertrans :> tigertrans = struct

open tigerframe
open tigertree
open tigertemp
open tigerabs

exception breakexc
exception divCero
	
type level = {parent:frame option , frame: frame, level: int} (* DUDA: aca nos sería level en vez de frame, pq funciona el typechecker? mariano *)
type access = tigerframe.access

type frag = tigerframe.frag
val fraglist = ref ([]: frag list)

val actualLevel = ref ~1 (* _tigermain debe tener level = 0. *)
fun getActualLev() = !actualLevel

val outermost: level = {parent=NONE,
	frame=newFrame{name="_tigermain", formals=[]}, level=getActualLev()}
fun newLevel{parent={parent, frame, level}, name, formals} =
	{
	parent=SOME frame,
	frame=newFrame{name=name, formals=formals},
	level=level+1}
fun allocArg{parent, frame, level} b = tigerframe.allocArg frame b
fun allocLocal{parent, frame, level} b = tigerframe.allocLocal frame b
fun formals{parent, frame, level} = tigerframe.formals frame

datatype exp =
	Ex of tigertree.exp
	| Nx of tigertree.stm
	| Cx of label * label -> tigertree.stm

fun seq [] = EXP (CONST 0)
	| seq [s] = s
	| seq (x::xs) = SEQ (x, seq xs)

fun unEx (Ex e) = e
	| unEx (Nx s) = ESEQ(s, CONST 0)
	| unEx (Cx cf) =
	let
		val r = newtemp()
		val t = newlabel()
		val f = newlabel()
	in
		ESEQ(seq [MOVE(TEMP r, CONST 1),
			cf (t, f),
			LABEL f,
			MOVE(TEMP r, CONST 0),
			LABEL t],
			TEMP r)
	end

fun unNx (Ex e) = EXP e
	| unNx (Nx s) = s
	| unNx (Cx cf) =
	(*	let
		val t = newlabel()
		val f = newlabel()
	in
		seq [cf(t,f),
			LABEL t,
			LABEL f]	
	end
	se puede hacer con un solo label:
	*)
	let 
	    val l = newlabel()
	    in seq[cf(l,l), LABEL l]
	end	
fun unCx (Nx s) = raise Fail ("Error (UnCx(Nx..))")
	| unCx (Cx cf) = cf
	| unCx (Ex (CONST 0)) =
	    (fn (t,f) => JUMP(NAME f, [f]))
	| unCx (Ex (CONST _)) =
	    (fn (t,f) => JUMP(NAME t, [t]))
	| unCx (Ex e) =
	    (fn (t,f) => CJUMP(NE, e, CONST 0, t, f))

(* Estas dos son para debug *)
fun Ir(e) =
	let	fun aux(Ex e) = tigerit.tree(EXP e)
		| aux(Nx s) = tigerit.tree(s)
		| aux _ = raise Fail "bueno, a completar!"
		fun aux2(PROC{body, frame}) = aux(Nx body)
		| aux2(STRING(l, "")) = l^":\n"
		| aux2(STRING("", s)) = "\t"^s^"\n"
		| aux2(STRING(l, s)) = l^":\t"^s^"\n"
		fun aux3 [] = ""
		| aux3(h::t) = (aux2 h)^(aux3 t)
	in	aux3 e end
fun nombreFrame frame = print(".globl " ^ tigerframe.name frame ^ "\n")

(* While y for necesitan la u'ltima etiqueta para un break *)
local
	val salidas: label option tigerpila.Pila = tigerpila.nuevaPila1 NONE
in
	val pushSalida = tigerpila.pushPila salidas
	fun popSalida() = tigerpila.popPila salidas
	fun topSalida() =
		case tigerpila.topPila salidas of
		SOME l => l
		| NONE => raise Fail "break incorrecto!"			
end

val datosGlobs = ref ([]: frag list)
fun procEntryExit{level: level, body} =
	let	val label = STRING(name(#frame level), "")
		val body' = PROC{frame= #frame level, body=unNx body}
		val final = STRING(";;-------", "")
	in	datosGlobs:=(!datosGlobs@[label, body', final]) end
fun getResult() = !datosGlobs

fun stringLen s =
	let	fun aux[] = 0
		| aux(#"\\":: #"x"::_::_::t) = 1+aux(t)
		| aux(_::t) = 1+aux(t)
	in	aux(explode s) end

(*fun stringExp(s: string) = Creo que esta versión que hizo guido está muy ligada al assembler particular y no es lo que pretende hacer el libro, hace que el interprete no imprima bien - Mariano
	let	val l = newlabel()
		val len = ".long "^makestring(stringLen s)
		val str = ".string \""^s^"\""
		val _ = datosGlobs:=(!datosGlobs @ [STRING(l, len), STRING("", str)])
	in	Ex(NAME l) end
*)
fun stringExp(s: string) =
	let	val l = newlabel()
		val _ = datosGlobs:=(!datosGlobs @ [STRING(l, s)])
	in	Ex(NAME l) end
fun preFunctionDec() =
	(pushSalida(NONE);
	actualLevel := !actualLevel+1)
fun functionDec(e, l, proc) = (* DUDA: por que devuelve un Ex si no se usa para nada? Mariano *)
	let	val body =
				if proc then unNx e
				else MOVE(TEMP rv, unEx e)
		val body' = procEntryExit1(#frame l, body)
		val () = procEntryExit{body=Nx body', level=l}
	in	Ex(CONST 0) end
fun postFunctionDec() =
	(popSalida(); actualLevel := !actualLevel-1)

fun unitExp() = Ex (CONST 0)

fun nilExp() = Ex (CONST 0)

fun intExp i = Ex (CONST i)

fun simpleVar(acc, nivel) =(*nivel de anidamiento, puede estar en otro frame*)
	case acc of
	    InReg r => Ex(TEMP r)
	    |InFrame k => (*k es el offset en el frame*)
		    let fun aux 0 = TEMP fp 
		    	| aux n = MEM(BINOP(PLUS,CONST fpPrev, aux(n-1)))
		    in Ex(MEM(BINOP(PLUS,CONST k,aux(!actualLevel-nivel)))) end
		(*COMPLETAdo*)


fun fieldVar(var, field) = 
	let val r = unEx var
        val t = newtemp() (*para evaluar r una sola vez*)
           (*debemos chequear que r no sea nil en tiempo de ejecucion*)        
    in 
        Ex(ESEQ(seq[MOVE(TEMP t,r),
                EXP(externalCall( "_checkNil",[TEMP t]))], (*las funciones que empizan con _ son de runtime*)
                MEM(BINOP(PLUS,TEMP t, CONST(wSz*field))))) (*todos los tipos ocupan lo mismo*)
    end
         (*COMPLETAdo*)


fun subscriptVar(arr, ind) =
let
	val a = unEx arr
	val i = unEx ind
	val ra = newtemp()
	val ri = newtemp()
in
	Ex( ESEQ(seq[MOVE(TEMP ra, a),
		     MOVE(TEMP ri, i),
		     EXP(externalCall("_checkIndexArray", [TEMP ra, TEMP ri]))],
		     MEM(BINOP(PLUS, TEMP ra, BINOP(MUL, TEMP ri, CONST tigerframe.wSz)))))
end
(*cuando wSz es pot de 2, se puede optimizar SHL, TEMP ti, log wSz*)


fun recordExp l = 
    (*deben estar ordenados por el numero de secuencia*)
    (*let val le = List.map (unEx o (#1)) l*)
    let val ls = Listsort.sort ( fn(x,y) => Int.compare(#2x, #2y) ) l
        fun unexp [] = []
        |    unexp ((e,i)::t) = (unEx e) :: (unexp t)
    in Ex (externalCall("_allocRecord",(CONST(length ls)):: (unexp ls) ) )
    end
    
	 (*COMPLETAdo*)

fun arrayExp{size, init} =
let
	val s = unEx size
	val i = unEx init
in
	Ex (externalCall("_initArray", [s, i])) (*tener en cuenta que aca no lo estamos guardando en ningun temporario*)
end

(*
fun arrayExp(exp1, exp2)=
	let 
	   val t1 = newtemp()
	   val t2 = newtemp()
	in
	  Ex(ESEQ(seq[MOVE(TEMP t1, unEx exp1),
			MOVE(TEMP t2, unEx exp2),
			MOVE(TEMP t1, CALL(NAME "_createArray",[TEMP t1, TEMP t2]))],
			TEMP t1))
			
*)

(*fun callExp (name,external,isproc,lev:level,ls) = *)
fun callExp(name, extern,isproc,level:level, params) = (*TODO*)
    (* evaluaremos los parámetros de izq a der *)
    let
       val staticlink = let fun aux 0 = TEMP fp 
		    	               | aux n = MEM(BINOP(PLUS,CONST fpPrev, aux(n-1)))
		                       in aux(getActualLev() - #level level+1) end    
       val params' =  if (not extern) then staticlink :: (List.map unEx params) else (List.map unEx params)
       fun paramtmps 0 = []
            | paramtmps n = (TEMP (newtemp()))::paramtmps (n-1)
       val tmpas = if (length params' - length tigerframe.argregs)<0  then [] else paramtmps (length params' - length tigerframe.argregs)        
       fun carga l [] = (l,[])
         | carga [] l = raise Fail "esto no deberia pasar jejeje2323"
         | carga (arg::t) (temp::s) = let val (nocargados,moves) = carga t s in (nocargados,MOVE(temp,arg)::moves) end
       val argsenreg = (List.map TEMP tigerframe.argregs) @ tmpas
       val (enstack,moves) = carga params' argsenreg (* asumimos enstack es vacio por ahora *) 
(* Luego se puede generar las instrucciones p/cargar en stack (ò mandar los temporarios) *)
(*
Finalmente se genera:
    CALL(nombre, listadelosparams)
*)in  
       (* print(name);print(Int.toString(length(argsenreg))); DEBUG Mariano *)
       Ex ( ESEQ (seq moves , CALL(NAME name,argsenreg)) ) 

  end
fun letExp ([], body) = Ex (unEx body)
 |  letExp (inits, body) = Ex (ESEQ(seq inits,unEx body))

fun breakExp() = 
	let val l = topSalida() 
    in
        Nx(JUMP(NAME l,[l]))
    end

fun seqExp ([]:exp list) = Nx (EXP(CONST 0))
    | seqExp (exps:exp list) =
		let
			fun unx [e] = []
				| unx (s::ss) = (unNx s)::(unx ss)
				| unx[] = []
		in
			case List.last exps of
				Nx s =>
					let val unexps = map unNx exps
					in Nx (seq unexps) end
				| Ex e => Ex (ESEQ(seq(unx exps), e))
				| cond => Ex (ESEQ(seq(unx exps), unEx cond))
		end

fun preWhileForExp() = pushSalida(SOME(newlabel()))

fun postWhileForExp() = (popSalida(); ())

fun whileExp {test: exp, body: exp, lev:level} =
    let
	    val cf = unCx test
	    val expb = unNx body
	    val (l1, l2, l3) = (newlabel(), newlabel(), topSalida())
    in
	    Nx (seq[LABEL l1, cf(l2,l3),
		    LABEL l2, expb,
		    JUMP(NAME l1, [l1]),
		    LABEL l3])
    end

fun forExp {lo, hi, var, body} =
    let val var' = unEx var 
	    val (l1,l2,sal) = (newlabel(),newlabel(), topSalida())
	in Nx(seq(case hi of 
			Ex(CONST n) => 
				if n<valOf(Int.maxInt) 
				    then [MOVE(var', unEx lo), 
				          JUMP(NAME l2, [l2]),
                				  LABEL l1, unNx body,
						  MOVE(var', BINOP(PLUS, var',CONST 1)),
						  LABEL l2, CJUMP(GT,var',CONST n, sal, l1),
						  LABEL sal]
			       else [MOVE(var', unEx lo), (*si es maxInt debe ejecutar body al menos una vez*)
				        LABEL l2,unNx body, CJUMP(EQ,var',CONST n, sal, l1),
				        LABEL l1,MOVE(var', BINOP(PLUS, var',CONST 1)), 
				        JUMP(NAME l2, [l2]),	
				        LABEL sal]
	
			| _ => 
			let val t = newtemp ()
			    in [MOVE(var', unEx lo), 
				    MOVE(TEMP t, unEx hi), 	
				    CJUMP(LE,var',TEMP t, l2,sal),
				    LABEL l2, unNx body,
				    CJUMP(EQ, TEMP t, var', sal, l1),
				    LABEL l1,MOVE(var', BINOP(PLUS, var',CONST 1)), 
				    JUMP(NAME l2, [l2]),	
				    LABEL sal]			
			    end))
	end 

fun ifThenExp{test, then'} =
	let val cj = unCx test(*lo desempaquetamos como una condicion*)
        val(l1,l2) = (newlabel(), newlabel())
    in
        Nx(seq[cj(l1,l2),LABEL l1,unNx then', LABEL l2]) (*if sin else no da ningun valor*)
    end 

fun ifThenElseExp {test,then',else'} =
	let val cj = unCx test(*lo desempaquetamos como una condicion*)
        val(l1,l2,l3) = (newlabel(), newlabel(),newlabel())
        val temp = newtemp()
    in
        Ex(ESEQ(seq[
                cj(l1,l2),
                LABEL l1, seq([MOVE(TEMP temp, unEx then'),JUMP(NAME l3,[l3])]),
                LABEL l2, MOVE(TEMP temp, unEx else'),
                LABEL l3],
            TEMP temp))   
    end
    
fun ifThenElseExpUnit {test,then',else'} =
		let val cj = unCx test(*lo desempaquetamos como una condicion*)
        val(l1,l2,l3) = (newlabel(), newlabel(),newlabel())
    in
        Nx(seq[cj(l1,l2),
                LABEL l1,seq([unNx then', JUMP(NAME l3, [l3])]), 
                LABEL l2,unNx else',
                LABEL l3]) (*if que no da ningun valor*)
    end  (*COMPLETAdo*)

fun assignExp{var, exp} =
    let
	    val v = unEx var
	    val vl = unEx exp
    in
	    Nx (MOVE(v,vl))
    end

fun varDec(acc,expi) = assignExp{var = simpleVar(acc, getActualLev()), exp=expi}

fun binOpIntExp {left, oper, right} = 
    let val expl = unEx left
        val expr = unEx right
        val t = newtemp()
    in Ex(BINOP( (case oper of
                    PlusOp => PLUS
                    | MinusOp => MINUS
                    | TimesOp => MUL
                    | DivideOp => DIV 
                    | _ => raise Fail ("error interno en binOpIntExp!")
            ), expl, expr) )  end
	(*COMPLETAdo*)

fun binOpIntRelExp {left,oper,right} =
    let val expl = unEx left
        val expr = unEx right
    in Cx( fn (t, f) => CJUMP( (case oper of
                    EqOp =>  EQ  
                    | NeqOp => NE
                    | LtOp => LT
                    | LeOp => LE
                    | GeOp => GE
                    | GtOp => GT
                    | _ => raise Fail ("error interno binOpIntRelExp!")
            ), expl, expr, t, f)) end
	 (*COMPLETAdo*)

fun binOpStrExp {left,oper,right} = (*se compara con una funcion de runtime*)
	let val expl = unEx left
        val expr = unEx right
    in Cx( fn (t, f) => CJUMP( (case oper of
                    EqOp =>  EQ  
                    | NeqOp => NE
                    | LtOp => LT
                    | LeOp => LE
                    | GeOp => GE
                    | GtOp => GT
                    | _ => raise Fail ("error interno binOpIntRelExp!")
            ), externalCall("_stringCompare",[expl, expr]),CONST 0, t, f)) end

end


