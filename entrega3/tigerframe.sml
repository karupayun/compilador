(*
	Frames para el 80386 (sin displays ni registers).

		|    argn    |	fp+4*(n+2)
		|    ...     |
		|    arg2    |	fp+16
		|    arg1    |	fp+12
		|  fp level  |  fp+8
		|  retorno   |	fp+4
		|   fp ant   |	fp
		--------------	fp
		|   local1   |	fp-4
		|   local2   |	fp-8
		|    ...     |
		|   localn   |	fp-4*n
*)

(*
cosas que dependen de la maquina, ej. cuantos bytes tiene una palabra, donde devuelve una funcion su resultado. En gral, 
este modulo conoce la maquina de destino pero no el lenguaje fuente.
*)

structure tigerframe :> tigerframe = struct

open tigertree

type level = int

val rax = "rax"
val rdx = "rdx"
val fp = (*string2temp*) "rbp"				(* frame pointer *)
val sp = (*string2temp*) "rsp"				(* stack pointer *)
val rv = (*string2temp*) "rax"			 	(* return value  *)
val ov = (*string2temp*) "rdx"				(* overflow value (edx en el 386) *)
val wSz = 8					(* word size in bytes *)
val log2WSz = 3				(* base two logarithm of word size in bytes *)
val fpPrev = 0				(* offset (bytes) del rbp anterior *)
val fpPrevLev = 2*wSz		(* offset (bytes) del static link*)
val argsInicial = 0			(* words cantidad de argumentos en stack inicialmente (por defecto) *) 
val argsOffInicial = 0		(* words *)
val argsGap = wSz			(* bytes desplazamiento de cada argumento *)
val regInicial = 1			(* reg  cantidad de argumentos en registros inicialmente*)  (* DUDA: por q esto es 1? no deberia ser 0? mariano*)
val localsInicial = ~1		(* words desplazamiento a partir de donde empiezan los locals *)
val localsGap = ~8 			(* bytes offset de cada local*)
val calldefs = [rv]         (* registros que son trasheados por la llamada a funcion *)
val specialregs = [rv, fp, sp] (* DUDA: para que sirven estos? mariano *)
val argregs = ["rdi","rsi","rdx","rcx","r8","r9"] (* registros donde van los primeros argumentos segun la convención de llamada *)
val callersaves = [] (* registros preservador por el invocador *) (* DUDA: que deberia ir aca? mariano *)
val calleesaves = ["rbx","rbp","rsp","r10","r15"] (* registros preservador por la funcion invocada *)
val calldefs = callersaves @ [rv]


type frame = {
	name: string,
	formals: bool list, (*si son escapadas*)
	locals: bool list,
	actualArg: int ref, (*ultimo arg generado*)
	actualLocal: int ref, (*ultimo local generado*)
	actualReg: int ref
}


type register = string
datatype access = InFrame of int | InReg of tigertemp.label
datatype frag = PROC of {body: tigertree.stm, frame: frame} (*text en assembler*)
	| STRING of tigertemp.label * string (*data en assembler*)
fun newFrame{name, formals} = {
	name=name,
	formals=formals,
	locals=[], (* DUDA: que es este campo? mariano *)
	actualArg=ref argsInicial, (* DUDA: que es este campo? mariano *)
	actualLocal=ref localsInicial,
	actualReg=ref regInicial (* DUDA: este campo es la cantidad de registros usados como argumentos? mariano *)
}
fun name(f: frame) = #name f
fun string(l, s) = l^tigertemp.makeString(s)^"\n"
(* old formals function *)
fun formals({actualArg=a, ...}: frame) = (*DUDA: que debe devolver esta funcion? mariano *)
	let	fun aux(n, m) = if m=0 then [] else InFrame(n)::aux(n+argsGap, m-1)
	in aux(argsInicial, !a) end
(* new formals function*)
(* TODO *)
fun maxRegFrame(f: frame) = !(#actualReg f)
(* old allocArg function *)
fun allocArg (f: frame) b = (* Generar un acceso ... *)
	case b of
	_ =>
		let	val ret = (!(#actualArg f)+argsOffInicial)*wSz
			val _ = #actualArg f := !(#actualArg f)+1
		in	InFrame ret end
	(* | false => InReg(tigertemp.newtemp()) *)
(* new allocArg function *)
(*fun allocArg (f:frame) b = TODO
    if !(#actualArg f) < length argregs then
        if b then
            let val dir = !(#actualLocal f)*wSz
                val _ = (#locals f) := (!(#locals f) @ [InFrame ret])
                val _ = (#actualArg f) := (!(#actualArg f)+1)
                val _ = (#actualLocal f) := (!(#actualLocal f)-1)
		    in InFrame ret end 
*)
fun allocLocal (f: frame) b = 
	case b of
	true =>
		let	val ret = InFrame(!(#actualLocal f)+localsGap)
		in	#actualLocal f:=(!(#actualLocal f)-1); ret end
	| false => InReg(tigertemp.newtemp())
fun exp(InFrame k) e = MEM(BINOP(PLUS, e, CONST k))
| exp(InReg l) e = TEMP l
fun externalCall(s, l) = CALL(NAME s, l)

fun procEntryExit1 (frame,body) = 
   
   body
end
