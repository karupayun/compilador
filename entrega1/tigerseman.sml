structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres

type expty = {exp: unit, ty: Tipo}

type venv = (string, EnvEntry) tigertab.Tabla
type tenv = (string, Tipo) tigertab.Tabla

val tab_tipos : (string, Tipo) Tabla = tabInserList(
	tabNueva(),
	[("int", (TInt RW)), ("string", TString)])

val tab_vars : (string, EnvEntry) Tabla = tabInserList(
	tabNueva(),
	[("print", Func{level=mainLevel, label="print",
		formals=[TString], result=TUnit, extern=true}),
	("flush", Func{level=mainLevel, label="flush",
		formals=[], result=TUnit, extern=true}),
	("getchar", Func{level=mainLevel, label="getstr",
		formals=[], result=TString, extern=true}),
	("ord", Func{level=mainLevel, label="ord",
		formals=[TString], result=(TInt RW), extern=true}),
	("chr", Func{level=mainLevel, label="chr",
		formals=[TInt RW], result=TString, extern=true}),
	("size", Func{level=mainLevel, label="size",
		formals=[TString], result=(TInt RW), extern=true}),
	("substring", Func{level=mainLevel, label="substring",
		formals=[TString, TInt RW, TInt RW], result=TString, extern=true}),
	("concat", Func{level=mainLevel, label="concat",
		formals=[TString, TString], result=TString, extern=true}),
	("not", Func{level=mainLevel, label="not",
		formals=[TInt RW], result=TInt RW, extern=true}),
	("exit", Func{level=mainLevel, label="exit",
		formals=[TInt RW], result=TUnit, extern=true})
	])

fun tipoReal (TTipo (s, ref (SOME (t)))) = tipoReal t
  | tipoReal t = t

fun tiposIguales (TInt _) (TInt _) = true
  | tiposIguales (TRecord _) TNil = true
  | tiposIguales TNil (TRecord _) = true 
  | tiposIguales (TRecord (_, u1)) (TRecord (_, u2 )) = (u1=u2)
  | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  | tiposIguales (TTipo (_, r)) b =
		let
			val a = case !r of
				SOME t => t
				| NONE => raise Fail "No debería pasar! (1)"
		in
			tiposIguales a b
		end
  | tiposIguales a (TTipo (_, r)) =
		let
			val b = case !r of
				SOME t => t
				| NONE => raise Fail "No debería pasar! (2)"
		in
			tiposIguales a b
		end
  | tiposIguales a b = (a=b)

fun transExp(venv, tenv) =
	let fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")
		fun cmptipo (t1,t2,nl) = (* Compara dos tipos, devolviendo uno si son iguales o error si son incomparables *)
			case (tiposIguales t1 t2) of
					false => error ("Tipos no comparables", nl)
				|	true => (case t1 of 
								(TInt _) => (TInt RW)
							|	TNil	 => t2
							|	_		 => t1)	
		fun trexp(VarExp v) = trvar(v)
		| trexp(UnitExp _) = {exp=(), ty=TUnit}
		| trexp(NilExp _)= {exp=(), ty=TNil}
		| trexp(IntExp(i, _)) = {exp=(), ty=TInt RW}
		| trexp(StringExp(s, _)) = {exp=(), ty=TString}
		| trexp(CallExp ({func,args},nl)) =
			let val (targs, tresult) = 
				case tabBusca (func, venv) of
					SOME (Func {formals, result, ...}) => (formals, result)
				|	_ => error (func^" no es una función", nl)
				val lteargs = List.map trexp args
				val ltargs = List.map (#ty) lteargs
			 	val _ = (List.map (fn (x,y) => cmptipo (x,y,nl)) (ListPair.zip (ltargs,targs))) (* Pablo: No me convence la función cmptipo, pero pasa *)
							handle Empty => error ("Número incorrecto de argumentos", nl) (* horrible con handle *)
			in
				{ty = tresult, exp = ()}
			end
		| trexp(OpExp({left, oper=EqOp, right}, nl)) =
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=(), ty=TInt RW}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=(), ty=TInt RW}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper, right}, nl)) = 
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr then
					case oper of
						PlusOp => if tipoReal tyl=TInt RW then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| MinusOp => if tipoReal tyl=TInt RW then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| TimesOp => if tipoReal tyl=TInt RW then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| DivideOp => if tipoReal tyl=TInt RW then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| LtOp => if tipoReal tyl=TInt RW orelse tipoReal tyl=TString then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| LeOp => if tipoReal tyl=TInt RW orelse tipoReal tyl=TString then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| GtOp => if tipoReal tyl=TInt RW orelse tipoReal tyl=TString then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| GeOp => if tipoReal tyl=TInt RW orelse tipoReal tyl=TString then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| _ => raise Fail "No debería pasar! (3)"
				else error("Error de tipos", nl)
			end
		| trexp(RecordExp({fields, typ}, nl)) =
			let
				(* Traducir cada expresión de fields *)
				val tfields = map (fn (sy,ex) => (sy, trexp ex)) fields

				(* Buscar el tipo *)
				val (tyr, cs) = case tabBusca(typ, tenv) of
					SOME t => (case tipoReal t of
						TRecord (cs, u) => (TRecord (cs, u), cs)
						| _ => error(typ^" no es de tipo record", nl))
					| NONE => error("Tipo inexistente ("^typ^")", nl)
				
				(* Verificar que cada campo esté en orden y tenga una expresión del tipo que corresponde *)
				fun verificar [] [] = ()
				  | verificar (c::cs) [] = error("Faltan campos", nl)
				  | verificar [] (c::cs) = error("Sobran campos", nl)
				  | verificar ((s,t,_)::cs) ((sy,{exp,ty})::ds) =
						if s<>sy then error("Error de campo", nl)
						else if tiposIguales ty t then verificar cs ds
							 else error("Error de tipo del campo "^s, nl)
				val _ = verificar cs tfields
			in
				{exp=(), ty=tyr}
			end
		| trexp(SeqExp(s, nl)) =
			let	val lexti = map trexp s
				val exprs = map (fn{exp, ty} => exp) lexti
				val {exp, ty=tipo} = hd(rev lexti)
			in	{ exp=(), ty=tipo } end
		| trexp(AssignExp({var=SimpleVar s, exp}, nl)) =
			{exp=(), ty=TUnit} (*COMPLETAR*)
		| trexp(AssignExp({var, exp}, nl)) =
			{exp=(), ty=TUnit} (*COMPLETAR*)
		| trexp(IfExp({test, then', else'=SOME else'}, nl)) =
			let val {exp=testexp, ty=tytest} = trexp test
			    val {exp=thenexp, ty=tythen} = trexp then'
			    val {exp=elseexp, ty=tyelse} = trexp else'
			in
				if tipoReal tytest=TInt RW andalso tiposIguales tythen tyelse then {exp=(), ty=cmptipo (tythen, tyelse,nl)} (* PROB: el tytest puede ser TInt RO *) 
				else error("Error de tipos en if" ,nl)
			end
		| trexp(IfExp({test, then', else'=NONE}, nl)) =
			let val {exp=exptest,ty=tytest} = trexp test
			    val {exp=expthen,ty=tythen} = trexp then'
			in
				if tipoReal tytest=TInt RW andalso tythen=TUnit then {exp=(), ty=TUnit} (* PROB: el tytest puede ser TInt RO *) 
				else error("Error de tipos en if", nl)
			end
		| trexp(WhileExp({test, body}, nl)) =
			let
				val ttest = trexp test
				val tbody = trexp body
			in
				if tipoReal (#ty ttest) = TInt RW andalso #ty tbody = TUnit then {exp=(), ty=TUnit}
				else if tipoReal (#ty ttest) <> TInt RW then error("Error de tipo en la condición", nl)
				else error("El cuerpo de un while no puede devolver un valor", nl)
			end
		| trexp(ForExp({var, escape, lo, hi, body}, nl)) =
			let val {ty = tyhi, exp} = trexp hi
			    val {ty = tylo, exp} = trexp lo
			    val _ = if tiposIguales tyhi (TInt RW) andalso tiposIguales tylo (TInt RW) 
						then ()
						else error ("Los límites del for deben ser expresiones enteras", nl)
				val venv' = tabInserta (var, Var {ty = TInt RO}, fromTab venv) (* Pablo: No entender esta línea *)
				val {ty = tybody, exp} = trexp (body) (* transExp (body, tenv, venv') *)
				val _ = if tybody <> TUnit then error ("El cuerpo del for debe ser de tipo unit", nl) else ()
			in
				{ty = TUnit, exp = ()}
			end
		| trexp(LetExp({decs, body}, _)) = 
			{exp=(), ty=TUnit} (*COMPLETAR*)
		| trexp(BreakExp nl) =
			{exp=(), ty=TUnit} (*COMPLETAR*)
		| trexp(ArrayExp({typ, size, init}, nl)) =
			{exp=(), ty=TUnit} (*COMPLETAR*)
		and trvar(SimpleVar s, nl) =
			{exp=(), ty=TUnit} (*COMPLETAR*)
		| trvar(FieldVar(v, s), nl) =
			{exp=(), ty=TUnit} (*COMPLETAR*)
		| trvar(SubscriptVar(v, e), nl) =
			{exp=(), ty=TUnit} (*COMPLETAR*)
		and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},pos)) = 
			(venv, tenv, []) (*COMPLETAR*)
		| trdec (venv,tenv) (VarDec ({name,escape,typ=SOME s,init},pos)) =
			(venv, tenv, []) (*COMPLETAR*)
		| trdec (venv,tenv) (FunctionDec fs) =
			(venv, tenv, []) (*COMPLETAR*)
		| trdec (venv,tenv) (TypeDec ts) =
			(venv, tenv, []) (*COMPLETAR*)
	in trexp end
fun transProg ex =
	let	val main =
				LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
								result=NONE, body=ex}, 0)]],
						body=UnitExp 0}, 0)
		val _ = transExp(tab_vars, tab_tipos) main
	in	print "bien!\n" end
	
end
