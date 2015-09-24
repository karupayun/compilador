(*completar traduccion de expresiones del AST a tree.stm o tree.exp. En gral, tigertrans no conoce la maquina de destino,
pero si el lenguaje fuente*)

structure tigertrans :> tigertrans = struct

open tigerframe
open tigertree
open tigertemp
open tigerabs

exception breakexc
exception divCero
	
type level = {parent:frame option , frame: frame, level: int}
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

fun stringExp(s: string) =
	let	val l = newlabel()
		val len = ".long "^makestring(stringLen s)
		val str = ".string \""^s^"\""
		val _ = datosGlobs:=(!datosGlobs @ [STRING(l, len), STRING("", str)])
	in	Ex(NAME l) end
fun preFunctionDec() =
	(pushSalida(NONE);
	actualLevel := !actualLevel+1)
fun functionDec(e, l, proc) =
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
		(*COMPLETAR*)


fun varDec(acc) = simpleVar(acc, getActualLev())

fun fieldVar(var, field) = 
	let val r = unEx var
        val t = newtemp() (*para evaluar r una sola vez*)
           (*debemos chequear que r no sea nil en tiempo de ejecucion*)        
    in 
        Ex(ESEQ(seq[MOVE(TEMP t,r),
                EXP(externalCall( "_checkNil",[TEMP t]))], (*las funciones que empizan con _ son de runtime*)
                MEM(BINOP(PLUS,TEMP t, CONST(wSz*field))))) (*todos los tipos ocupan lo mismo*)
    end
         (*COMPLETAR*)


fun subscriptVar(arr, ind) =
let
	val a = unEx arr
	val i = unEx ind
	val ra = newtemp()
	val ri = newtemp()
in
	Ex( ESEQ(seq[MOVE(TEMP ra, a),
		MOVE(TEMP ri, i),
		EXP(externalCall("_checkindex", [TEMP ra, TEMP ri]))],
		MEM(BINOP(PLUS, TEMP ra,
			BINOP(MUL, TEMP ri, CONST tigerframe.wSz)))))
end



(*
let val t = newtemp()
	    val ti = newtemp()
	in 
	    ESEQ(seq[MOVE(TEMP t, unEx e), 
		    EXP(CALL(NAME "checkIndex", [TEMP t, TEMP ti])],
		    MEM(BINOP(PLUS,TEMP t, BINOP(MUL, CONST wSz, TEMP ti)))) (*cuando wSz es pot de 2, se puede optimizar SHL, TEMP ti, log wSz*)
*)

fun recordExp l = Ex(CONST 0)
    (*deben estar ordenados por el numero de secuencia*)
    (*Ex(*CONST*)(CALL(NAME "_createRecord",(CONST(length l))::List.map (unEx o #1) l))
    ó let val t = newtemp()
    in Ex(ESEQ(MOVE(TEMP t, CALL("_createRecord",...)), TEMP t)) end
*)
    
	 (*COMPLETAR*)

fun arrayExp{size, init} =
let
	val s = unEx size
	val i = unEx init
in
	Ex (externalCall("allocArray", [s, i]))
end

(*
fun arrayExp(exp1, exp2)=
	let 
	   val t1 = newtemp()
	   val t2 = newtemp()
	in
	  Ex(ESEQ(Seq[MOVE(TEMP t1, unEx exp1),
			MOVE(TEMP t2, unEx exp2),
			MOVE(TEMP t1, CALL(NAME "_createArray",[TEMP t1, TEMP t2]))],
			TEMP t1))
			
SON NECESARIOS ESTOS TEMP?			
*)

(*fun callExp (name,external,isproc,lev:level,ls) = *)
fun callExp(name, params) = (*TODO*)
    (* evaluaremos los parámetros de izq a der *)
    let val params' = List.map unEx params
     (*   fun paramtmps 0 = []
            | paramtmps n = (TEMP (newtemp()))::paramtmps (n-1)
        val tmpas = if (length params - length argregs)<0  then [] else paramtmps (length params - lenth argregs) (* ACÁ HAY QUE ARREGLAR EL CASO NEGATIVO *) ! Unbound value identifier: argregs (esta definido en tigerframe!!)
        
        fun carga l [] = l
            | carga (arg::t) (temp::s) = ( MOVE(temp,arg) )::carga t s
        fun pzip l [] = List.map (fn(x) => (x,NONE)) l
        |    pzip [] _ = []
        |   pzip (h::t) (r::s) = (h,SOME r)::pzip t s
        (*val empareja = pzip params' (argregs @ tmpas) Unbound value identifier: argregs (esta definido en tigerframe!!)*)
        val enstack = List.filter (fn(_,NONE) => True | _ => False) empareja *)
(* Luego se puede generar las instrucciones p/cargar en stack (ò mandar los temporarios)
Finalmente se genera: 
    CALL(nombre, listadelosparams)
*)in 
    	Ex (CONST 0)
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
				if n<valOf(Int.minInt) then [MOVE(var', unEx lo), (*CUANDO PUEDE SER MENOR QUE minINT?*)
							     JUMP(NAME l2, [l2]),
							     LABEL l1, unNx body,
							   MOVE(var', BINOP(PLUS, var',CONST 1)),
								LABEL l2, CJUMP(GT,var',CONST n, sal, l1),
								LABEL sal]
			       else [MOVE(var', unEx lo),
				   LABEL l2, CJUMP(GT,var',CONST n, sal, l1),
				   unNx body, LABEL l1,MOVE(var', BINOP(PLUS, var',CONST 1)), 
				   JUMP(NAME l2, [l2]),	
				   LABEL sal]
	
			| _ => [])) (*VER!*)(*let val t = newtemp ()
			
		          in [MOVE(var', unEx lo), 
				    MOVE(TEMP t, unEx hi), (***)	
				    CJUMP(LE(var',TEMP t, l2,sal),
				    LABEL l2, unNx body,
				    CJUMP(EQ, TEMP t, var', sal, l1),
				    LABEL l1,MOVE(var', BINOP(PLUS, var',CONST 1)), 
				    JUMP(NAME l2, [l2]),	
				    LABEL sal)]			
			end))*)
	end (*COMPLETAR*)

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
    in Ex (CONST 0) (*VER!
        Ex(ESEQ(seq[
                cj(l1,l2),
                LABEL l1, SEQ(MOVE(TEMP tmp, unEx then')),JUMP(LABEL l3,[l3]),
                LABEL l2, SEQ(MOVE(TEMP tmp, unEx else')),
                LABEL l3],
            TEMP tmp))   *)
    end

fun ifThenElseExpUnit {test,then',else'} =
	Ex (CONST 0) (*COMPLETAR*)

fun assignExp{var, exp} =
    let
	    val v = unEx var
	    val vl = unEx exp
    in
	    Nx (MOVE(v,vl))
    end

fun binOpIntExp {left, oper, right} = 
	Ex (CONST 0) (*COMPLETAR*)

fun binOpIntRelExp {left,oper,right} =
	Ex (CONST 0) (*COMPLETAR*)

fun binOpStrExp {left,oper,right} =
	Ex (CONST 0) (*COMPLETAR*)

end


