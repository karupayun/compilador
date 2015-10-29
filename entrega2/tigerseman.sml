structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open tigerpp
open tigertrans
open tigertemp

type expty = {exp: unit, ty: Tipo}

type venv = (string, EnvEntry) tigertab.Tabla
type tenv = (string, Tipo) tigertab.Tabla

val levelPila: tigertrans.level tigerpila.Pila = tigerpila.nuevaPila1(tigertrans.outermost)
val mainLevel: tigertrans.level = tigertrans.outermost
 
fun pushLevel l = tigerpila.pushPila levelPila l
fun popLevel() = tigerpila.popPila levelPila 
fun topLevel() = tigerpila.topPila levelPila

fun zip3 ([], [], []) = []
  | zip3 ((b::bs), (c::cs), (d::ds)) = (b,c,d) :: zip3 (bs, cs, ds) 				                    
  | zip3 _= raise Fail "caca2"
              
fun zip5 [] [] [] [] [] = []
  | zip5 (b::bs) (c::cs) (d::ds) (e::es) (f::fs) = (b,c,d,e,f) :: zip5 bs cs ds es fs
  | zip5 _ _ _ _ _  = raise Fail "caca"
           

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

fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")

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

fun cmptipo t1 t2 nl = (* Compara dos tipos, devolviendo uno si son iguales o error si son incomparables *)
			case (tiposIguales t1 t2) of
					false => error ("Tipos no comparables", nl)
				|	true => (case t1 of 
								(TInt _) => (TInt RW)
							|	TNil	 => t2
							|	_		 => t1)	

fun transExp(venv, tenv) =
	let fun trexp(VarExp v) = trvar(v)
		| trexp(UnitExp _) = {exp=unitExp(), ty=TUnit}
		| trexp(NilExp _)= {exp=nilExp(), ty=TNil}
		| trexp(IntExp(i, _)) = {exp=intExp i, ty=TInt RW}
		| trexp(StringExp(s, _)) = {exp= stringExp s, ty=TString}
		| trexp(CallExp ({func,args},nl)) =
			let val (targs, tresult, flabel, fexterna,flevel) = 
				case tabBusca (func, venv) of
					SOME (Func {formals, result,label,extern,level}) => (formals, result,label,extern,level)
				    |	_ => error (func^" no es una función", nl)
                val largs = List.map trexp args
				val ltargs = List.map (#ty) largs
                val leargs = List.map (#exp) largs 
                val _ = if (List.length args) > (List.length targs) then error(func^" demasiados argumentos",nl) 
                        else if List.length args < List.length targs then error(func^" argumentos insuficientes",nl) else ()
    		 	val _ = (List.map (fn (x,y) => cmptipo x y nl) (ListPair.zip (ltargs,targs))) 
			 			(*handle Empty => error ("Número incorrecto de argumentos", nl) (* TEST: Probar este handle *) ListPair.zip no tira excepciones*)
			 	val isproc = (tresult=TUnit)		
			in
				{ty = tresult, exp = callExp(flabel,fexterna,isproc,flevel,leargs)} 
			end
		| trexp(OpExp({left, oper=EqOp, right}, nl)) =
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit 
				then {exp = if tiposIguales tyl TString then binOpStrExp {left=expl,oper=EqOp,right=expr} else binOpIntRelExp {left=expl,oper=EqOp,right=expr}, ty=TInt RW}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit 
				then {exp=if tiposIguales tyl TString then binOpStrExp {left=expl,oper=NeqOp,right=expr} else binOpIntRelExp {left=expl,oper=NeqOp,right=expr}, ty=TInt RW}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper, right}, nl)) = 
			let
				val {exp=expl, ty=tyl} = trexp left
				val {exp=expr, ty=tyr} = trexp right
				 
				
			in
				if tiposIguales tyl tyr then
					case oper of
						PlusOp => if tiposIguales tyl (TInt RW) then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt RW} else error("Error de tipos PlusOp", nl)
						| MinusOp => if tiposIguales tyl (TInt RW) then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt RW} else error("Error de tipos MinusOp", nl)
						| TimesOp => if tiposIguales tyl (TInt RW) then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt RW} else error("Error de tipos TimesOp", nl)
						| DivideOp => if tiposIguales tyl (TInt RW) then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt RW} else error("Error de tipos DivideOp", nl)
						| LtOp => if tiposIguales tyl (TInt RW) 
						          then {exp = binOpIntRelExp {left=expl,oper=oper,right=expr}, ty =TInt RW}
						          else if tiposIguales tyl TString then {exp=binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt RW} 
						                                           else error("Error de tipos <", nl)
						| LeOp => if tiposIguales tyl (TInt RW) 
						          then {exp = binOpIntRelExp {left=expl,oper=oper,right=expr}, ty =TInt RW}
						          else if tiposIguales tyl TString then {exp=binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt RW} 
						                                           else error("Error de tipos <=", nl)
						| GtOp => if tiposIguales tyl (TInt RW) 
						          then {exp = binOpIntRelExp {left=expl,oper=oper,right=expr}, ty =TInt RW}
						          else if tiposIguales tyl TString then {exp=binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt RW} 
						                                           else error("Error de tipos >", nl)
						| GeOp => if tiposIguales tyl (TInt RW) 
						          then {exp = binOpIntRelExp {left=expl,oper=oper,right=expr}, ty =TInt RW}
						          else if tiposIguales tyl TString then {exp=binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt RW} 
						                                           else error("Error de tipos >=", nl)						| _ => raise Fail "No debería pasar! (3)"
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
				fun verificar [] [] = []
				  | verificar (c::cs) [] = error("Faltan campos", nl)
				  | verificar [] (c::cs) = error("Sobran campos", nl)
				  | verificar ((s,t,p)::cs) ((sy,{exp,ty})::ds) =
						if s<>sy then error("Error de campo", nl)
						else if tiposIguales ty (!t) then (exp,p)::verificar cs ds
							 else error("Error de tipo del campo "^s, nl)
			    val cs_sorted = Listsort.sort ( fn(x,y) => String.compare (#1x,#1y) ) cs
			    val tfields_sorted = Listsort.sort ( fn(x,y) => String.compare (#1x,#1y) ) tfields
				val lf = verificar cs_sorted tfields_sorted (* TEST: Testear que ahora no importa el orden *)
			in
				{exp=recordExp lf, ty=tyr}
			end
		| trexp(SeqExp(s, nl)) =
			let	val lexti = map trexp s
				val exprs = map (fn{exp, ty} => exp) lexti
				val {exp, ty=tipo} = hd(rev lexti)
			in	{ exp=seqExp (exprs), ty=tipo } end
		| trexp(AssignExp({var, exp}, nl)) =
		    let val {exp=expv, ty=tyv} = trvar(var,nl)
		        val {exp=expe, ty=tye} = trexp(exp)
                val _ = if (tiposIguales tyv tye) then () else error("los tipos no coinciden en la asignacion",nl)                
                val _ = if tyv = TInt RO then error("Intentando asignar una variable RO",nl) else () 
			in {exp=assignExp{var = expv, exp =expe}, ty=TUnit}  
			end
		| trexp(IfExp({test, then', else'=SOME else'}, nl)) =
			let val {exp=testexp, ty=tytest} = trexp test
			    val {exp=thenexp, ty=tythen} = trexp then'
			    val {exp=elseexp, ty=tyelse} = trexp else'
			in
				if tiposIguales tytest (TInt RW) andalso tiposIguales tythen tyelse 
				then {exp=if tiposIguales tythen TUnit then ifThenElseExpUnit {test=testexp,then'=thenexp,else'=elseexp} else ifThenElseExp {test=testexp,then'=thenexp,else'=elseexp}, ty = cmptipo tythen tyelse nl} 
				else error("Error de tipos en if-then-else" ,nl)
			end
		| trexp(IfExp({test, then', else'=NONE}, nl)) =
			let val {exp=exptest,ty=tytest} = trexp test
			    val {exp=expthen,ty=tythen} = trexp then'
			in
				if tiposIguales tytest (TInt RW) andalso tythen=TUnit then {exp=ifThenExp{test=exptest, then'=expthen}, ty=TUnit} 
				else error("Error de tipos en if-then", nl)
			end
		| trexp(WhileExp({test, body}, nl)) =
			let
				val {exp = te', ty = tt'} = trexp test
				val _ = preWhileForExp () (*la llamamos despues de hacer trexp test para que esto ande bien: while (break;1) do ()*)
				val {exp = be', ty = bt'} = trexp body
				(*chequeos*)
				val ev' = whileExp {test = te', body = be', lev = topLevel() (*no se usa lev*)}
				val _ = postWhileForExp()
			in
			    if not (tiposIguales (tt') (TInt RW)) then error("Error de tipo en la condición", nl)
			    else if  bt' <> TUnit then error("El cuerpo de un while no puede devolver un valor", nl)
			    else {exp=ev', ty=TUnit}
			end
		| trexp(ForExp({var, escape, lo, hi, body}, nl)) = (* TEST *)
			let val {ty = tyhi, exp = exphi} = trexp hi
			    val {ty = tylo, exp =explo} = trexp lo
			    val _ = if tiposIguales tyhi (TInt RW) andalso tiposIguales tylo (TInt RW) then ()
						else error ("Los límites del for deben ser expresiones enteras", nl)
				val vacc = allocLocal (topLevel()) (!escape)		
				val vlvl = getActualLev()
				val venv' = tabRInserta (var, Var {ty = TInt RO, access = vacc, level = vlvl}, venv) (* TEST *) 
                val _ = preWhileForExp()
				val {ty = tybody, exp = expb} = transExp(venv', tenv) body
				val _ = if tybody <> TUnit then error ("El cuerpo del for debe ser de tipo unit", nl) else ()
                val _ = postWhileForExp()
			in
				{ty = TUnit, exp = forExp{lo = explo, hi = exphi, var = simpleVar(vacc, vlvl), body = expb}}
			end
		| trexp(LetExp({decs, body}, _)) = (*VER!*)
		    let fun aux (d, (t, v, exps1)) =
				let
					val (t', v', exps2) = transDec (t, v,[], [d])
				in
					(t', v', exps1@exps2)
				end
        
				val (tenv', venv', expdecs) = List.foldl aux (tenv, venv, []) decs (*transDec(tenv,venv,[],decs)*)
				val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
			in 
				{exp=seqExp(expdecs@[expbody]), ty=tybody}
			end			
		| trexp(BreakExp nl) =
			(({exp = breakExp(), ty=TUnit})
                handle Fail s => error(s,nl))
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
                val _ = if tiposIguales (!ta) tyi then () else error("La expresion inicializadora no es un "^typ,nl)(*no habria que imprimir !ta?*)
			in {exp=arrayExp{size = exps,init = expi}, ty=TArray (ta,ur)} end 
		and trvar(SimpleVar s, nl) =  
			    ( case tabBusca(s,venv) of
                       SOME t => (case t of 
								  Var {ty,access, level} => {exp=simpleVar(access,level), ty=ty} 
								| _ => error(s^" no es una variable",nl) )
                       | _ => error("Variable "^s^" no definida",nl) )
		| trvar(FieldVar(v, s), nl) = 
	        let 
	            val {exp=expv, ty=tyv} = trvar(v,nl)
	            val (t,pos) = ( case tyv of
	                         TRecord (l,_) => ( case List.filter (fn x => #1x = s ) l of
	                                             [] => error("Record no tiene campo "^s,nl)
	                                             | (e::_) => (#2e,#3e) )
	                         | _ => (tigerpp.ppTipo(tyv) ; error("No se puede indexar porque no es Record",nl)) )
	            in {exp=fieldVar(expv,pos), ty=(!t)} end
		| trvar(SubscriptVar(v, e), nl) = 
	        let
	            val {exp=expe, ty=te} = trexp e
	            val {exp=expv, ty=tv} = trvar(v,nl)
            val _ = if tiposIguales te (TInt RW) then () else error("Indice debe ser entero",nl)
	            in  case tv of
	                     TArray (t,_) => {exp=subscriptVar(expv, expe), ty=(!t)}
	                     | _ => error("Indexando algo que no es un arreglo", nl) end
        in trexp end

and transDec(tenv,venv,el,[]) = (tenv,venv,el)
  | transDec(tenv,venv,el, (VarDec ({name, escape, typ=NONE, init},nl))::t) =
        let val {exp=expi,ty=tyi} = transExp(venv,tenv) init
            val _ = if tyi = TNil then error(" inicializando la variable "^name^" con nil sin estar tipada",nl) else ()
            val venv' = tabRInserta(name, Var {ty=tyi, access = allocLocal (topLevel()) (!escape), level = getActualLev()}, venv)
            in transDec(tenv,venv',el,t) end
  | transDec(tenv,venv,el, (VarDec ({name, escape, typ=SOME syty, init},nl))::t) =
        let val {exp=expi,ty=tyi} = transExp(venv,tenv) init
            val rt = ( case tabBusca(syty,tenv) of
                       NONE => error("Tipo "^syty^" indefinido",nl) (* TEST: no se puede hacer algo como var x:NIL := nil de alguna forma sucia? *)
                     | SOME tyi' => if tiposIguales tyi' tyi then cmptipo tyi' tyi nl else error("La expresion asignada no es del tipo esperado "^syty,nl) ) (* TEST: Se puede asignar nil a un record? *)
           val venv' = tabRInserta(name, Var {ty=rt, access = allocLocal (topLevel()) (!escape), level = getActualLev()}, venv)
            in transDec(tenv,venv',el,t) end
   | transDec(tenv,venv,el, (FunctionDec lf)::t) = 
	    let fun searchTy nl syty = 
                    (case tabBusca(syty,tenv) of
                           NONE => error("Tipo "^syty^" indefinido",nl)
                         | SOME ty => ty )
            val funcName = map (#name o #1) lf
	        val ocurrenciasNombres = List.map (fn(x) => List.length (List.filter (fn(y)=>y=x) funcName)) funcName
	        val _ = List.foldl (  fn(x,y)=> if x=1 then y+1 else error("Nombres de funciones repetidos en el mismo batch", #2 (List.nth(lf,y)) )  ) 0 ocurrenciasNombres  (* chequeo que no haya nombres repetido, hay un foldl para llevar un recuento del índice y acceder al número de línea *)
            val argsNombres = map ((map #name) o #params o #1) lf

            val retTipo = map (  fn(func,nl) => Option.getOpt(Option.map (searchTy nl) (#result func),TUnit)  ) lf
			fun procField [] _ = []
        	   | procField ({name, escape=_, typ=(NameTy syty)}::xs) nl = (searchTy nl syty) :: (procField xs nl)
               | procField _ _ = raise Fail "creo que esto no deberia pasar 22!" (* TEST seguro que eso no pasa? *)
   			val argsTipos = map ( fn(func, nl) => procField (#params func) nl ) lf
            val escapes = map ( fn({params,...}, nl) => (map (! o  (#escape)) params)) lf
            val labels = map (fn({name,...},nl) => tigertemp.newlabel()^" "^name^" "^(Int.toString(nl))) lf (* Etiquetas para debuguear *) 
		    val levels = map (fn(l,f) => newLevel{parent=topLevel(),name=l ,formals = f }) (ListPair.zip(labels, escapes))	        
            val venv' =  List.foldl (fn((lab,lev,fname,argT,retT),v) => tabRInserta(fname,Func {level=lev, label=lab, formals=argT, result=retT,extern=false},v)   ) venv (zip5 labels levels funcName argsTipos retTipo)       
            
            val funcsArgsTiposEscapes = map zip3 (zip3(argsTipos,argsNombres,escapes))
            
            
            fun procF (argsTiposEscapes, level, nl, body, tRet) = let val _ = preFunctionDec()
                                                                      val _ = pushLevel level
                                                                      val myenv = foldl (fn((argT,argN,escape),v)=>(tabRInserta(argN, Var {ty=argT, access=allocArg level escape, level=getActualLev()  }, v))) venv' argsTiposEscapes
                                                                      val {ty=fTy,...} = transExp(myenv,tenv) body
                                                                      val _ = if tiposIguales tRet fTy then () else (error("La funcion no devuelve el tipo con el que se la declara",nl))
                                                                      val _ = popLevel()
                                                                      val _ = postFunctionDec()
                                                                  in () end      
            val nlS = map #2 lf
            val funcBodies = map (#body o #1) lf            
            val _ = map procF (zip5 funcsArgsTiposEscapes levels nlS funcBodies retTipo)
         in transDec(tenv,venv',el,t) end
    | transDec(tenv, venv, el, (TypeDec lt)::t) = 
        let 
            val tiposName = map (#name o #1) lt
            val ocurrenciasNombres = List.map (fn(x) => List.length (List.filter (fn(y)=>y=x) tiposName)) tiposName
            val _ = List.foldl (  fn(x,y)=> if x=1 then y+1 else error("Nombres de tipos repetidos en el mismo batch", #2 (List.nth(lt,y)) )  ) 0 ocurrenciasNombres  (* chequeo que no haya nombres repetido, hay un foldl para llevar un recuento del índice y acceder al número de línea *)            
            val tenv' = tigertopsort.fijaTipos (map (#1) lt) tenv
                handle noExiste => raise Fail ("Error: tipo inexistente en el batch")
                     | Ciclo => raise Fail ("Error: hay un ciclo")
            in transDec(tenv', venv, el, t) end	       
   
   
(* TEST var x := while ... 
*)

fun transProg ex =
	let	val main =
				LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
								result=SOME "int", body=ex}, 0)]],
						body=UnitExp 0}, 0)
		val {ty,exp} = transExp(tab_vars, tab_tipos) main
	in	print "allok\n" end
	
end

