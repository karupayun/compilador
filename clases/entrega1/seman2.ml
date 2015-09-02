(* Etapa 5 : Análisis Semántico (parte 2) - Modulo topsort *)

Veremos ahora como tratar las declaraciones de tipos de Tiger. Definimos los tipos internos del compilador:

structure tigertips =
struct

type unique = unit ref
datatype Tipo = TUnit
	| TNil
	| TInt of RW
	| TString
	| TArray of Tipo * unique
	| TRecord of (string * Tipo * int) list * unique
	| TFunc of Tipo list * Tipo
	| TTipo of string * Tipo option ref

end

La idea central es que, una vez procesado el batch de declaraciones de tipos, no deben quedar TTipo (* Tipo usado para no definido *)

Lo primero será detectar si hay ciclos. Para esto usaremos un sort topológico.
Def: Dado un conjunto {u1, u2, ... , un} de elementos y un conjunto de pares {(pred,suc)} de la forma (ui,uj), un sort topológico tratará de encontrar una secuencia (no necesariamente única) de

uk1,uk2,...,ukn  tal que , si ∃ (u1,uj) => ki < kj

Para facilitar las cosas:

structure tiger topsort ...

fun topsort p ....


Ahora debemos, dado un batch de declaraciones generar los pares predecesor/sucesor.

Nota: un nombre de tipo (a la izquierda) tiene que estar una sola vez.
Consideraciones: La complicación acá viene dada por los records, que pueden tener ciclos. Además, se pueden hacer cadenas largas.

Ej:
	let
		type A = B
		type B = C
		type C = D
		type D = {i:int, next: A}
	in
		0
	end
	
Para conseguir esto, definimos:


				Sintaxis			declaración				Pares generados
			type name = n		{name, ty = NameTy n}		(n,name)
		type name = array of n	{name, ty = ArrayTy n}		(n,name)
		type name = record{..}	{name, ty = RecordTy lf}	   nada				


fun buscaArr ...

fun genPares ...

fun procesa ...

Ejemplos:
1) (* incorrecto *)
let
	type A = B
	type B = A
	type C = {i:int, next = A}
in 
	0
end

2) (* correcto *)
let
	type A = B
	type B = C
	type C = {i:int, next = A}
	var ra:a := nil
	var rb:b := nil
	var rc:c := nil
in 
	ra := rb
	ra := rc
	rb := rc
	0
end

3) (* correcto *)
let
	type R = {h:int, i:int, j:string, k:R}
	var r:= R {i = ord ("A"), h = 34, j = "hola", k = nil)
in 
	0
end

4) (* incorrecto *)
let
	type R = {h:int, i:int, j:string}
	var r:= R {i = 10, h = 11, j = 12)
in 
	0
end

5) (* correcto *)
let
	type R = {i:int}
	var x:= 0
	type T = R
	var y:= 1
	type A = array of R
in 
	0
end

6) (* correcto *)
let
	type R = {i:int, j:int, k:int}
	var r:= R {i = 10, h = 11, j = 12)
in 
	r.i := 10;
	r.j := 10;
	r.k := 10;
	0
end

7) (* correcto *)
let
	type R1 = {i:int, next = R2}
	type R2 = {i:int, next = R1}
	var r:R1 := R1{i = 10, next = R2 {i = 11, next = R1 {i = 12, next = nil}}}
in
	0
end

8) (* correcto *)
let
	type tree = {key:int, children : treelist}
	type treelist = {hd:tree, tl = treelist}
	var r:treelist{hd = tree{key=10, children = treelist {hd = nil, tl = nil}}}
in
	r.hd.children.hd.key
end

9) (* correcto *)
let
	type R1 = {i:int, next = R2}
	type R2 = {i:int, next = R1}
	type A = array of R1
	var a:= A[10] of {i = 10, next = R2 {i = 11, next = R1 {i = 12, next = nil}}}
in
	a[5].next.next.next.next.next.next.next.next.i
end

10) (* correcto *)
let
	type A = B
	type B = C
	type C = {i:int, next = A}
	type Arr = array of A
in 
	0
end

11) (* correcto *) 
let
	type A = B
	type B = {next:B}
in 
	0
end

12) (* incorrecto *)
let
	type R1 = {i:int}
	var r1:R1:=nil
	type R2 = {i:int}
	var r2:R2:=nil
in
	r1 := if 2 > 3 then nil else r2;
	0
end
