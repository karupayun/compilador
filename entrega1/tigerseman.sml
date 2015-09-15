structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open tigerpp

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

fun tiposIguales (TInt _) (TInt _) = true
  | tiposIguales (TRecord _) TNil = true
  | tiposIguales TNil (TRecord _) = true 
  | tiposIguales (TRecord (_, u1)) (TRecord (_, u2 )) = (u1=u2)
  | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  | tiposIguales (TTipo _) b =
		(* let *)
		(* 	val a = case !r of *)
		(* 		SOME t => t *)
		(* 		| NONE => raise Fail "No debería pasar! (1)" *)
		(* in *)
		(* 	tiposIguales a b *)
		(* end *)raise Fail "No debería pasar! (1)"
  | tiposIguales a (TTipo _) =
		(* let *)
		(* 	val b = case !r of *)
		(* 		SOME t => t *)
		(* 		| NONE => raise Fail "No debería pasar! (2)" *)
		(* in *)
		(* 	tiposIguales a b *)
		(* end *)raise Fail "No debería pasar! (1)"
  | tiposIguales a b = (a=b)

fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")
fun transExp(venv, tenv) =
	let fun cmptipo t1 t2 nl = (* Compara dos tipos, devolviendo uno si son iguales o error si son incomparables *)
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
				val ltargs = List.map ((#ty) o trexp) args
			 	val _ = (List.map (fn (x,y) => cmptipo x y nl) (ListPair.zip (ltargs,targs))) 
			 			handle Empty => error ("Número incorrecto de argumentos", nl) (* TEST: Probar este handle *) 
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
						PlusOp => if tiposIguales tyl (TInt RW) then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| MinusOp => if tiposIguales tyl (TInt RW) then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| TimesOp => if tiposIguales tyl (TInt RW) then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| DivideOp => if tiposIguales tyl (TInt RW) then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| LtOp => if tiposIguales tyl (TInt RW) orelse tiposIguales tyl TString then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| LeOp => if tiposIguales tyl (TInt RW) orelse tiposIguales tyl TString then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| GtOp => if tiposIguales tyl (TInt RW) orelse tiposIguales tyl TString then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| GeOp => if tiposIguales tyl (TInt RW) orelse tiposIguales tyl TString then {exp=(),ty=TInt RW} else error("Error de tipos", nl)
						| _ => raise Fail "No debería pasar! (3)"
				else error("Error de tipos", nl)
			end
		| trexp(RecordExp({fields, typ}, nl)) =
			let
				(* Traducir cada expresión de fields *)
				val tfields = map (fn (sy,ex) => (sy, trexp ex)) fields

				(* Buscar el tipo *)
				val (tyr, cs) = case tabBusca(typ, tenv) of
					SOME t => (case t of (*Tipo Real *)
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
			    val cs_sorted = Listsort.sort ( fn(x,y) => String.compare (#1x,#1y) ) cs
			    val tfields_sorted = Listsort.sort ( fn(x,y) => String.compare (#1x,#1y) ) tfields
				val _ = verificar cs_sorted tfields_sorted (* TEST: Testear que ahora no importa el orden *)
			in
				{exp=(), ty=tyr}
			end
		| trexp(SeqExp(s, nl)) =
			let	val lexti = map trexp s
				val exprs = map (fn{exp, ty} => exp) lexti
				val {exp, ty=tipo} = hd(rev lexti)
			in	{ exp=(), ty=tipo } end
		| trexp(AssignExp({var=SimpleVar s, exp}, nl)) = 
		    let val {exp=expv, ty=tyv} = trvar(SimpleVar s,nl)
		        val {exp=expe, ty=tye} = trexp(exp)
		        val _ = cmptipo tyv tye 
			in {exp=(), ty=TUnit} 
			end
		| trexp(AssignExp({var, exp}, nl)) =
		    let val {exp=expv, ty=tyv} = trvar(var,nl)
		        val {exp=expe, ty=tye} = trexp(exp)
		        val _ = cmptipo tyv tye 
			in {exp=(), ty=TUnit} 
			end
		| trexp(IfExp({test, then', else'=SOME else'}, nl)) =
			let val {exp=testexp, ty=tytest} = trexp test
			    val {exp=thenexp, ty=tythen} = trexp then'
			    val {exp=elseexp, ty=tyelse} = trexp else'
			in
				if tiposIguales tytest (TInt RW) andalso tiposIguales tythen tyelse then {exp=(), ty=cmptipo tythen tyelse nl} 
				else error("Error de tipos en if-then-else" ,nl)
			end
		| trexp(IfExp({test, then', else'=NONE}, nl)) =
			let val {exp=exptest,ty=tytest} = trexp test
			    val {exp=expthen,ty=tythen} = trexp then'
			in
				if tiposIguales tytest (TInt RW) andalso tythen=TUnit then {exp=(), ty=TUnit} 
				else error("Error de tipos en if-then", nl)
			end
		| trexp(WhileExp({test, body}, nl)) =
			let
				val ttest = trexp test
				val tbody = trexp body
			in
			    if not (tiposIguales (#ty ttest) (TInt RW)) then error("Error de tipo en la condición", nl)
			    else if #ty tbody <> TUnit then error("El cuerpo de un while no puede devolver un valor", nl)
			    else {exp=(), ty=TUnit}
			end
		| trexp(ForExp({var, escape, lo, hi, body}, nl)) = (* TEST *)
			let val {ty = tyhi, ...} = trexp hi
			    val {ty = tylo, ...} = trexp lo
			    val _ = if tiposIguales tyhi (TInt RW) andalso tiposIguales tylo (TInt RW) then ()
						else error ("Los límites del for deben ser expresiones enteras", nl)
				val venv' = tabRInserta (var, Var {ty = TInt RO}, venv) (* TEST *)
				val {ty = tybody, ...} = transExp(venv', tenv) body
				val _ = if tybody <> TUnit then error ("El cuerpo del for debe ser de tipo unit", nl) else ()
			in
				{ty = TUnit, exp = ()}
			end
		| trexp(LetExp({decs, body}, _)) =
			let
				val (tenv', venv', _(*ef. lat. de cod. intermedio*)) = transDec(tenv,venv,[],decs)
				val {exp,ty=tybody}=transExp(venv', tenv') body
			in
				{exp=(), ty=tybody}
			end
			
			
		| trexp(BreakExp nl) =
			{exp=(), ty=TUnit}
		| trexp(ArrayExp({typ, size, init}, nl)) =
            let
                val {exp=exps,ty=tys} = trexp size
                val {exp=expi,ty=tyi} = trexp init
                val _ = if tiposIguales tys (TInt RO) then () else error("El size del arreglo no es entero",nl)
                val (ta,ur) = ( case tabBusca(typ,tenv) of
                                  SOME t => ( case t of
                                                  TArray (ta',ur') => (ta',ur')
                                                | _ => error("El tipo "^typ^" no es un arreglo",nl) )
                                  | _ => error("Tipo "^typ^" no definido",nl) )
                val _ = if tiposIguales ta tyi then () else error("La expresion inicializadora no es un "^typ,nl)
			in {exp=(), ty=TArray (ta,ur)} end
		and trvar(SimpleVar s, nl) =  
			    ( case tabBusca(s,venv) of
                       SOME t => (case t of 
								  Var {ty} => {exp=(), ty=ty} 
								| _ => error(s^" no es una variable",nl) )
                       | _ => error("Variable "^s^" no definida",nl) )
		| trvar(FieldVar(v, s), nl) =
	        let 
	            val {exp=expv, ty=tyv} = trvar(v,nl)
	            val t = ( case tyv of
	                         TRecord (l,_) => ( case List.filter (fn x => #1x = s ) l of
	                                             [] => error("Record no tiene campo"^s,nl)
	                                             | (e::_) => #2e )
	                         | _ => error("No se puede indexar porque no es Record",nl) )
	            in {exp=(), ty=t} end
		| trvar(SubscriptVar(v, e), nl) =
	        let
	            val {exp=expe, ty=te} = trexp e
	            val {exp=expv, ty=tv} = trvar(v,nl)
            val _ = if tiposIguales te (TInt RW) then () else error("Indice debe ser entero",nl)
	            in  case tv of
	                     TArray (t,_) => {exp=(), ty=t}
	                     | _ => error("Indexando algo que no es un arreglo", nl) end
        in trexp end
(* CODIGO VIEJO: Como trdec toma venv y tenv no deberiamos ponerla adentro de transExp y deberiamos llamarla transDec *)
(* trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},pos)) = 
	(venv, tenv, []) (*COMPLETAR*)
| trdec (venv,tenv) (VarDec ({name,escape,typ=SOME s,init},pos)) =
	(venv, tenv, []) (*COMPLETAR*)
| trdec (venv,tenv) (FunctionDec fs) =
	(venv, tenv, []) (*COMPLETAR*)
| trdec (venv,tenv) (TypeDec ts) =
	(venv, tenv, []) (*COMPLETAR*) *)
and transDec(tenv,venv,el,[]) = (tenv,venv,el)
  | transDec(tenv,venv,el, (VarDec ({name, escape, typ=NONE, init},nl))::t) =
        let val {exp=expi,ty=tyi} = transExp(venv,tenv) init
            val venv' = tabRInserta(name, Var {ty=tyi}, venv)
            in transDec(tenv,venv',el,t) end
  | transDec(tenv,venv,el, (VarDec ({name, escape, typ=SOME syty, init},nl))::t) =
        let val {exp=expi,ty=tyi} = transExp(venv,tenv) init
            val _ = ( case tabBusca(syty,tenv) of
                       NONE => error("Tipo "^syty^" indefinido",nl) (* TEST: no se puede hacer algo como var x:NIL := nil de alguna forma sucia? *)
                     | SOME tyi' => if tiposIguales tyi' tyi then () else error("La expresion asignada no es del tipo esperado "^syty,nl) )
            val venv' = tabRInserta(name, Var {ty=tyi}, venv)
            in transDec(tenv,venv',el,t) end
   | transDec(tenv,venv,el, (FunctionDec lf)::t) = (* Esta funcion esta siendo debuggeada *)
	    let fun searchTy nl syty = 
                    (case tabBusca(syty,tenv) of
                           NONE => error("Tipo "^syty^" indefinido",nl)
                         | SOME ty => ty )
            fun procField [] _ = []
                | procField ({name, escape=_, typ=(NameTy syty)}::xs) nl = (searchTy nl syty) :: (procField xs nl)
                | procField _ _ = raise Fail "creo que esto no deberia pasar 22!" (* TEST seguro que eso no pasa? *)
            val funcName = map (#name o #1) lf
	        val ocurrenciasNombres = List.map (fn(x) => List.length (List.filter (fn(y)=>y=x) funcName)) funcName
	        val _ = List.foldl (  fn(x,y)=> if x=1 then y+1 else error("Nombres de funciones repetidos en el mismo batch", #2 (List.nth(lf,y)) )  ) 0 ocurrenciasNombres  (* chequeo que no haya nombres repetido, hay un foldl para llevar un recuento del índice y acceder al número de línea *)
            val argsNombres = map ((map #name) o #params o #1) lf
            val argsTipos = map ( fn(func, nl) => procField (#params func) nl ) lf
            val retTipo = map (  fn(func,nl) => Option.getOpt(Option.map (searchTy nl) (#result func),TUnit)  ) lf
            val venvWithFuncs =  List.foldl (   fn((fname,(argT, retT)),v) => tabRInserta(fname,Func {level=(), label="", formals=argT, result=retT,extern=false},v)   ) venv (ListPair.zip(funcName,ListPair.zip(argsTipos, retTipo)))
            val funcsArgsTipos = map ListPair.zip (ListPair.zip(argsTipos,argsNombres))
            val funcvEnvs = map (foldl (fn((argT,argN),v)=>tabRInserta(argN, Var {ty=argT}, venv)) venvWithFuncs) funcsArgsTipos
            val nlS = map #2 lf
            val funcBodies = map (#body o #1) lf
            val funcsTrans =  map (fn(fEnv,fBody) => transExp(fEnv,tenv) fBody) (ListPair.zip(funcvEnvs,funcBodies))
            val _ = List.app (  fn(nl,(retT,fTy)) => if tiposIguales retT fTy then () else (error("La funcion no devuelve el tipo con el que se la declara",nl)) ) (ListPair.zip(nlS,ListPair.zip(retTipo,map #ty funcsTrans))) 
        in transDec(tenv,venvWithFuncs,el,t) end
    | transDec(tenv,venv,el,_) = raise Fail "TODO"
(* TEST var x := while ... 
*)

fun transProg ex =
	let	val main =
				LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
								result=SOME "int", body=ex}, 0)]],
						body=UnitExp 0}, 0)
		val _ = transExp(tab_vars, tab_tipos) main
	in	print "bien!\n" end
	
end
