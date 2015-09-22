(* Etapa 4 : Análisis Semántico *)

Esta etapa tomará el AST y lo examinará, chequeando que esté bien tipado.
Nota: Tiger es un compilador broad, excepto en esta etapa, que se hace en paralelo con la generación de código intermedio. 
	  Para facilitar las cosas, diferiremos la generación de código.
	 
En particular, durante la etapa de análisis se maneja un record del tipo {exp, ty} (* {Codigo intermedio, tipo} *)
Para evitar el impacto de un cambio grande definiremos en alguna parte un datatype para exp como el que sigue:
datatype exp = SCAF (* scaffolding *)

Ahora si, al análisis semántico. Necesitamos representar los tipos posibles que se usarán. 
Ej: Necesitamos un tipo para expresiones como:
	while cond do exp
	if cond then exp (* sin else *)    (* No pueden estar en asignaciones *)
	
Necesitamos dar un tipo que preventa usarlas en asign.

En tigertips.sml
structure tigertips =
struct
(* datatype 'a ref = ref of 'a. Ej:
	val r = red 10  ---> int *r = malloc (sizeof 10); *r = 10;
	val _ = !r  	---> *r
	val _ = r := 11	---> *r = 11
*)
type unique = unit ref
datatype Tipo =
	  TUnit (* No asignable *)
	| TNil
	| TInt of read
	| TString
	| TArray of Tipo * unique
	| TRecord of (string * Tipo * int) list * unique
	| TFunc of Tipo list * Tipo
	| TTipo of string * Tipo option ref
and read = RO | RW
end

OBS1: Problema: Los array y los records, ¿Cuándo tendrán el mismo tipo?

Esto determinará cuándo y qué operaciones se les podrán aplicar.

a1 := a2  (* Dos arreglos *) --> Obvio, cuando tengan el mismo tipo.

Propuesta1 (C): Dos valores tienen el mismo tipo si tienen la misma estructura

struct A {int i; double d} s1;
struct B {int j; double f} s2;
s1 = s2; (* Correcto! *)

Propuesta2 (Pascal): Dos tipos son iguales si vienen de la misma definición. Usaremos esta.

type A = definicion1
type B = definicion1

Pero... Tiger permite redefinir tipos.

let
	type A = array of int
	var a := A[10] of 0
	type A = array of int
	var b := A[10] of 1
in
	a := b (* incorrecto *)
	0
end

OBS2: pos en TRecord permitirá esto:

let
	type R = {i:int, t:text, next:R}
	var r:= R{next = nil, t = "hola", i = 1}
in 0 end

OBS3: 
let
	type A = B
	type B = C
	...
	type Z = int

OBS4:
	ML tiene tuplas (1,true) y records {i = 1, b = true}, #i{i = 1, b = true} = 1
	En realidad las tuplas son records:
		{1 = 10, 2 = true, 3 = "hola"} -> (10,true,"hola")
		#2 (10, true, "hola") = true

Necesitamos dos diccionarios (espacios de cambios), uno para variables y funciones y otro para los tipos. Los llamaremos venv y tenv.

Para venv definimos:

datatype EnvEntry =
			VarEntry of {ty:Tipo}
		|	FunEntry of {format:Tipo list, result:Tipo}

venv : (string, EnvEntry) Tabla
Inicialmente venv será:

[("print", FunEntry {formals = [TString], result = TUnit}),
 ("getchar", FunEntry {formals = [], result = TString}),
 ("not", FunEntry {formals = [TInt RW], result = TInt}), (* Duda: Con este TInt hay que tener cuidado? *)
 ...]
 
tenv : (string, Tipo) e inicialmente tendrá

[("int", TInt RW),
 ("string", TString)]
 
El modulo semántico exportará:  (* tigerseman.sml *)
transExp : exp*tenv*venv -> {exp,ty}
transDec : dec*tenv*venv -> tenv * venv
transTy : ty*tenv -> ty
transVar : var*tenv*venv -> {exp,ty}

Bosquejo del código de transExp

Las funciones de tigerseman son violentamente recursivas. Pero, la mayoría de las veces, tranExp no cambia sus entornos.
Para evitar pasar entornos sin necesidad, definimos dentro de transExp a 
trexp : exp -> {exp,ty}

(* tigerseman.sml *)
fun transExp(venv, tenv) =
	fun trexp(IntExp(i, _)) = {exp=SCAF, ty=TInt RW}
	| trexp(StringExp(s, _)) = {exp=SCAF, ty=TString}
	| trexp(NilExp (s, _))= {exp=SCAF, ty=TNil}
	| trexp(IfExp({test, then', else'=NONE}, nl)) =
			let val {exp=SCAF,ty=tytest} = trexp test
			    val _ = if tytest <> (TInt RW) andalso tytest <> (TInt RO)
						then error ("test no entero", nl)
						else ()
			    val {exp=SCAF,ty=tythen} = trexp then'
			    val _ = if tythen <> TUnit then error ("then no unit",nl) else ()
			in
				{exp=SCAF, ty=TUnit}
			end
	| trexp(IfExp({test, then', else'=SOME else'}, nl)) =
			let val {exp=testexp, ty=tytest} = trexp test
				val _ = if tytest <> (TInt RW) andalso tytest <> (TInt RO)
					then error ("test no entero", nl)
					else ()
			    val {exp=thenexp, ty=tythen} = trexp then'
			    val {exp=elseexp, ty=tyelse} = trexp else'
			in
				val t = cmptipo tythen tyelse then {exp=(), ty=tythen}
				else error("then y else no coinciden en tipos" ,nl)
			end
		
donde 
fun cmptipo (t1,t2,nl) =
	case (t1,t2) of
		(TInt _, TInt _) => TInt RW
	|	(TString, TString) => TString
	| 	(t as (TArray (_,u1), TArray (_,u2))) => if u1=u2 then t else error ("tipos en arreglos",nl)
	| 	(t as (TRecord (_,u1), TRecord (_,u2))) => if u1=u2 then t else error ("tipos en records",nl)
	| 	(t as (TRecord _), TNil) => t
	| 	(TNil, t as (TRecord _)) => t
	|	_ => error ("Tipos no iguales", nl)
	

		|	
		| trexp(ForExp({var, escape, lo, hi, body}, nl)) =
			let val {ty = tyhi, exp} = trexp hi
			    val {ty = tylo, exp} = trexp lo
			    val _ = if cmptipo (tyhi, TInt, nl1)= TIntRW andalso cmptipo (tylo, TInt, nl2)= TIntRW
						then ()
						else error (...)
				val venv' = tabInserta (var, VarEntry {ty = TInt RO}, fromTabla venv)
				val {ty = tybody, exp} = transExp (body, tenv, venv')
				val _ = if tybody <> TUnit then error (...) else ()
			in
				{ty = TUnit, exp = SCAF}
		| trexp(CallExp ({func,args},nl)) =
			let val (targs, tresult) = 
				case tabBusca (func, venv) of
					SOME (FunEntry {formals, result}) => (formals, result)
				|	_ = > error (func^" no es función", nl)
				val lteargs = List.map trexp args
				val ltargs = List.map (#ty) lteargs
				val _ = (List.map (fn (x,y) => cmptipo (x,y,nl)) (Listpair.zip (ltargs,targs)))
							Handle Empty => error ("nro de argumentos", nl)
			in
				{ty = tresult, exp = SCAF}
			end
			
			
Nota: Los operadores & y | se implementan (a nivel de AST) con ifs:
	A & B == if A then B else 0
	A | B == if A then 1 else B

con lo que estos operadores tienen semántica lazy (en la jerga de C, semántica de cortocircuito) (* Si A es true corta el or *)


		| trexp (OpExp ({left,right,oper},nl)) 

Debemos tomar los casos según las operaciones:
* PlusOp, MinusOp, TimesOp, DivideOp solo se aplican a enteros (TInt _), se deben chequear los tipos de left y right
let
	val {exp=_, ty=tyl} = trexp left
	val {exp=_, ty=tyr} = trexp right
	val _ = (* tyleft y tiright son TInt _ *)
in
	{exp = SCAF, ty = TInt RW}
end

* GtOp, GeOp, etc ... se pueden aplicar a (TInt _) y a TString (en orden lexicográfico según el código ASCII). En ambos casos el valor resultante es 
{exp = SCAF, ty = TInt RW}

* EqOp, NeOp ( = <> ), que se pueden aplicar a TInt _, TString, TArray, TRecord, TNil, se pueden comparar:

TInt _ , TInt _
TString, TString
TArray, TArray
TRecord, TRecord
TNil, TRecord (y al revés)
No se pueden comparar TNils !!!

La semántica de comparación de arreglos y records es si apuntan a la misma dirección de memoria (a lo Java).

Ej:
let 
	type A = array of int
	var a1 := a[5] of 15
	var a2 := a[5] of 15
	var a3 := a2
	(* a1 <> a2 y a3 = a2 *)
	
Detalle de implementación.
Si es posible, el runtime se hará en C, para aprovechar las facilidades de libc. 
En UNIX, el startup se linkea para producir un ejecutable (el startup colecta argumentos, entornos, etc). El startup luego invoca a main.
Para dejar contento al startup, definiremos un main en el runtime; de acá tendremos que pasar al código de tiger.

El main tendrá esta pinta:

int main
{
	código antes de tigermain
	return tigermain (0);
}

Así que deberemos poder fabricar un _tigermain en algún momento. La haremos en el seman con el sgte. sucio truco.

fun seman expast =
	let	val le =
				LetExp({decs=[FunctionDec[({name="_tigermain", formals=[],
								result=NONE, body=expast}, 0)]],
						body=UnitExp}, 0)
	in	transexp le end
end

** Pasemos al LetExp

		| trexp(LetExp({decs, body}, nl)) =
			let
				val (tenv', venv', _(*ef. lat. de cod. intermedio*)) = transDec(decs,tenv, venv) decs
				val {exp,ty=tybody}=transExp (body, venv', tenv') 
			in 
				{exp=SCAF, ty=tybody}
			end
		and transDec (tenv, venv, el, []) = (tenv,venv,el)
		|	transDec (tenv, venv, el, (VarDec({name, escape, ty = NONE, init}, nl) = (* var x := 10 *)
				let val {exp,ty} = transExp (init, tenv, venv)
					val venv' = tabInserta (name, VarEntry {ty = ty}, venv)
				in transDec (tenv,venv',el,t) end
		|	transDec (tenv, venv, el, (VarDec({name, escape, ty = SOME (NameTy t), init}, nl) = 
				let val {exp,ty} = transExp (init, tenv, venv)
					val _ = case tabBuscar (t,tenv) =>
							NONE => raise Fail (t^"tipo no existe")
						|	SOME ty' => if cmptipo (ty, ty') then ()
										else error ("Tipos no coinciden")
					val venv' = tabInserta (name, VarEntry {ty = ty}, venv)
				in transDec (tenv,venv',el,t) end
		|	transDec (tenv, venv, el, (FunctionDec lf::t)) = (* lf batch *)
		
Deberemos hacer el análisis en dos etapas, por recursión mutua.
1) Generar e insertar en venv las FunEntry correspondientes a las funciones
2) Con el venv aumentado, analizar los body de las funciones

1-- Se implementa con:
		val lpr = map (fn ({name, params, result, body}, pos) =>
								(name, FunEntry, {formals = map transTy params, result = transTy result})) lf
		val venv' = List.foldr (fn ((n,fe,env) => tabInserta (n,fe,env))) venv lpr
2-- Con el venv' anterior se analizar los body de las funciones:

val et = map (fn ({body, ...}) => transExp (tenv,venv',body)) lf

Ahora lo más picante: Declaraciones de tipos.

Recordemos que las declaraciones de tipos vienen en batches, y en un catch pueden ser mutuamente recursivos. No puede haber ciclor. Vamos a detectarlos.

Ej:
let 
	type A = B
	type B = C
	type C = {i:int, prox = A}
in
...
¿Que tenv tendría que quedar?

("A", TRecord [("i", .., TInt RW), ref())


			
