(* Etapa 0 : Introducción *)

Un compilador involucra 3 lenguajes:
- Lenguaje Fuente que se quiere compilar (Tiger)
- Lenguaje Objeto, el destino de la compilación (a elegir)
- Lenguaje de implementación (ML -- Moscow ML)

ML
(* Comentario *)
int 
bool
char #"a"
string "hola"
Comparación = (NO == )
Constructor :: 1::[2] (no 1:[2] )
Concat: [1]@[2]
Asignación: val x = 1
Funciones  fun f x = 2*x

No necesita indentación
Es EAGER. No es tan puro como haskell.

if .. then .. else, let .. in .. end, no tiene where

\x -> 2*x === fn x => 2*x
(f) === op f

data D = A | B Int === datatype D = A | B of int

f x = g x 
g x = f x
====
fun f x = g x
and g x = f x

Pureza de ML:
Tiene un datatype llamado ref
type 'a ref = ref of 'a

Se porta como un puntero

val r = ref 0 	=== int *c; c = malloc (sizeof 0); *c = 0;
!r (= 0) 		=== *c
r := 1			=== *c = 1


TIGER

Lenguaje imperativo.
Tipos: Int y String
tipado estático fuerte
Permite funciones anidadas
Tiene arreglos y records.
Permite definir nuevos tipos que pueden ser mutuamente recursivos.
Tiene las estructuras de control clásicasa.

SINTAXIS
Toda definición, si la hay, de tipos, variables o funciones va en un bloque let

let
	declaracion
in
	expresiones
end

El resultado del bloque es el de la última expresión

Declaraciones de variables -- var nombre:= valor   o   var nombre:tipo := valor

En el 1er tipo se infiere del valor asignado (hay una excepción).
Se puede definir más de una vez una variable. Las nuevas definiciones ocultan la anterior.

let var x:= let var x:= 1 in x+x end
in x end


me canse ... Si alguno lo completa ...
