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

val fp = (*string2temp*) "FP"				(* frame pointer *)
val sp = (*string2temp*) "SP"				(* stack pointer *)
val rv = (*string2temp*) "RV"			 	(* return value  *)
val ov = (*string2temp*) "OV"				(* overflow value (edx en el 386) *)
val wSz = 4					(* word size in bytes *)
val log2WSz = 2				(* base two logarithm of word size in bytes *)
val fpPrev = 0				(* offset (bytes) *)
val fpPrevLev = 8			(* offset (bytes) *)
val argsInicial = 0			(* words *) (*PARCHE*)
val argsOffInicial = 0		(* words *)
val argsGap = wSz			(* bytes *)
val regInicial = 1			(* reg *) 
val localsInicial = 0		(* words *)
val localsGap = ~4 			(* bytes *)
val calldefs = [rv]
val specialregs = [rv, fp, sp]
val argregs = []
val callersaves = []
val calleesaves = []

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
	locals=[],
	actualArg=ref argsInicial,
	actualLocal=ref localsInicial,
	actualReg=ref regInicial
}
fun name(f: frame) = #name f
fun string(l, s) = l^tigertemp.makeString(s)^"\n"
fun formals({actualArg=a, ...}: frame) = 
	let	fun aux(n, m) = if m=0 then [] else InFrame(n)::aux(n+argsGap, m-1)
	in aux(argsInicial, !a) end
fun maxRegFrame(f: frame) = !(#actualReg f)
fun allocArg (f: frame) b = (*PARCHE*) (* Generar un acceso ... *)
	case b of
	_ =>
		let	val ret = (!(#actualArg f)+argsOffInicial)*wSz
			val _ = #actualArg f := !(#actualArg f)+1
		in	InFrame ret end
	(* | false => InReg(tigertemp.newtemp()) *)
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
