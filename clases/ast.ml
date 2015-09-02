(* Etapa 3 : AST *)

El parser entrega un AST, cuyos constructores están en tigerabs.
Nota: El sistema de modulos de SML
Un módulo (en SML una estructura) es un container con valores y funciones. Ej:

Structure S = 
struct 
	type t = int
	val x = 10
	fun f x = 2 * x
end

Toda estructura tiene un tipo o signatura. En particular, la estructura S tiene una signatura:

sig
	type t = int
	val x : int
	val f : int -> int
end

Se le puede impoer una signatura a una estructura (*DUDA: Que es una signatura *)
Ej: 

signature SG =
sig
	type t (* opaco, solo se vera a la estructura *) (*DUDA: Y esto?*)
	val f : int -> int
end

Structure s :> SG =
struct
	type t = int
	val x = 10 (* x no se ve *)
	fun f x = 2 * x
end

También podemos hacerlo así

Structure S:
sig
	type t
	val f: int -> int 
end =
struct
	...
end


MosML recién implementó las signaturas de SML en la última versión. Antes usaba este esquema:

(* archivo cosa.sig *)
signature cosa = 
sig 
	val f: string -> int
	val g: int -> string
end

y luego (* cosa.ml *) (* Duda: Tengo que entender esto? *)
structure cosa :> cosa = 
struct 
	fun f s = valOF (StringtoInt s)  (* valOF: a' option -> 'a *) 
	fun g n = InttoString n
end

Si tenemos estructuras y signaturas, debiéramos tener funciones (functores)

Ej: Quicksort
fun qs [] = []
|	qs (h::t) = let val (m,M) List.partition (fn x => x < h) t
				in (qs m)@[h]@(qs M) end

El tipo que infiere el compilador es:
val qs : int list -> int list

Podemos generalizarlo pasando un segundo argumento.
fun qs [] _ = []
|	qs (h::t) cmp = let val (m,M) List.partition (fn x => cmp (x,h)) t
				in (qs m cmp)@[h]@(qs M cmp) end
				
Ahora qs: 'a list -> ('a x 'a -> bool) -> 'a list				

Pero ahora hay que darle dos valores a qs. Una alternativa puede ser:

functor qs (s:sig  type t val cmp:t*t->Bool end):
sig
	val qs : s.t list -> s.t list
end =
struct
	fun qs ...
end

Usándose:
structure QSint : QS (struct type t = int
					  val cmp = Fn(x,y) -> x < y end)
QSint.qs[9,8,7,6]

Nota2: ML no tiene listas por comprensión como Haskell o Erlang. Pero ..

fun mapend _ [] = []
|	mapend f (h::t) = f h @ mapend f t

mapend :: ('a -> 'b list) -> 'a list -> 'b list

Ahora, en erlang:
perms [] -> []
perms L -> [[X|Y] || X <- L, Y <- perms (L -- [X])]

En ML, con mapend y definiendo en --

infix --
fun []--_ = []
|	(h::t) -- x = if x=h then t else h::(t--x)

fun perms [] = [[]]
|	perms L = mapend (fn x => (mapend (Fn y => x::y) perms (L--x)) ) L

Volvamos: Tenemos el AST del fuente tig de ese momento. Debemos calcular que variables escapan.

Ej1:
function f (i:int) =
	let function g() = print(chr(i))
in g() end

f y g tendrán sus propios marcos de activación. g accede a i, que está en otro marco de activación (el de F).
Así que recorremos el AST llevando el reegistro de las variables escapadas.

Ej2:
let function f (i:int,j:int) = 
	let function g(j:int) =
		print (chr(i*j))
	in g(10)
in f(30,40)

i escapa, j NO escapa

Para hacer esto necesitamos un tipo de datos que permita guardar tuplas. (clave, valor). Usaremos una estructura tigertab. Su signatura se parece a esto:


type (''a, 'b) Tabla (* '' debe permitir igualdad *) (* var, escape x prof: String, bool x int *) 
val tabInserta : (''a,'b,(''a, 'b) Tabla) -> (''a, 'b) Tabla
val fromTabla : (''a, 'b) Tabla -> (''a, 'b) Tabla
val tabNueva : unit -> (''a, 'b) Tabla
val tabBusca : (''a,(''a, 'b) Tabla) -> 'b option
val tabALista : (''a, 'b) Tabla -> ''a * b list

Como recorrer el AST:
(* DUDA : No entiendo esta parte *)
fun fescapes (IntExp _) env prof = () 
|	fescapes (NilExp _) _ _ = () 
|	fescapes (IfExp ({test, then', else' = NONE}, _) env prof =	
		(fescapes test env prof;fescapes then' env prof)
|	fescapes (VarExp (var, nl)) env prof =
		case tabBusca (var,env) of
			NONE => raise Fail ("variable "^var^" no existe!")
		|	SOME (escape,p) => if p < prof then escape := true else ()
|	fescapes (LetExp ({decs,body},_) env prof = 
		fescapes body (fdecs decs en prof) prof

and fdecs (VarDec {name, escape, init, ...},_) env prof =
		(fescapes init env prof; tabInserta (name, (escape, prof), env))
|	fdecs (FunctionDec (lst) env prof = 
		let fun fdec ({name, params, body, ...}, env) =
		let val env' = foldr (fn {name, escape, ...} => (tabInserta (name, (escape,prof))))
		in fescapes body env' (prof+1) end
	in fold (fdec env lst) end
	
	


