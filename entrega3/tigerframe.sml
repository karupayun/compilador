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

fun seq [] = EXP (CONST 0)
	| seq [s] = s
	| seq (x::xs) = SEQ (x, seq xs)

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
val localsInicial = 0		(* words desplazamiento a partir de donde empiezan los locals *)
val localsGap = ~8 			(* bytes offset de cada local*)
val calldefs = [rv]         (* registros que son trasheados por la llamada a funcion *)
val specialregs = [rv, fp, sp] (* DUDA: para que sirven estos? mariano *)
val argregs = ["rdi","rsi","rdx","rcx","r8","r9"] (* registros donde van los primeros argumentos segun la convención de llamada *)
val callersaves = [] (* registros preservador por el invocador *) (* DUDA: que deberia ir aca? mariano *)
val calleesaves = ["rbx","r10","r15"] (* registros preservador por la funcion invocada *)
val calldefs = callersaves @ [rv]

datatype access = InFrame of int | InReg of tigertemp.temp
type register = string
type frame = {
	name: string,
	formals: bool list, (*si son escapadas*)
	argsAcc: access list ref, (*lista de argumentos*)
	actualLocal: int ref (*offset en words del proximo potencial local*)
}
datatype frag = PROC of {body: tigertree.stm, frame: frame} (*text en assembler*)
	| STRING of tigertemp.label * string (*data en assembler*)
fun allocLocal (f: frame) b = 
	case b of
	true =>
		let	val ret = InFrame(!(#actualLocal f)+localsGap)
		in	#actualLocal f:=(!(#actualLocal f)-1); ret end
	| false => InReg(tigertemp.newtemp())

fun newFrame{name, formals} =let val f = { name=name,
                                       	   formals=formals,
                                           argsAcc = ref ([]:access list),
	                                       actualLocal=ref localsInicial }
                                 val _ = #argsAcc f := List.map (fn b => allocLocal f b) formals
                             in f  end
fun name(f: frame) = #name f
fun string(l, s) = l^tigertemp.makeString(s)^"\n"
(* old formals function *)
fun formals({argsAcc, ...}: frame) = !argsAcc
fun exp(InFrame k) efp = MEM(BINOP(PLUS, efp, CONST k))
| exp(InReg l) e = TEMP l
fun externalCall(s, l) = CALL(NAME s, l)

fun procEntryExit1 ({argsAcc, ...}: frame,body) = 
   let fun aux [] _ = []
       |   aux (acc::accs) n = MOVE( exp acc (TEMP fp), if n < List.length argregs then TEMP (List.nth(argregs,n)) else MEM(BINOP(PLUS, CONST ((n-List.length argregs)*8+fpPrevLev), TEMP fp)) ) :: aux accs (n+1)
       val moveargs = aux (!argsAcc) 0 (*Instrucciones para mover de los argumentos a los locals donde la función ve internamente las cosas *)
       val freshtmps = List.tabulate (List.length calleesaves , fn _ => TEMP (tigertemp.newtemp()))
       val saveregs = List.map MOVE (ListPair.zip(freshtmps,List.map TEMP calleesaves)) (* Instrucciones para salvar en temporarios los callee saves *)
       val restoreregs = List.map MOVE(ListPair.zip(List.map TEMP calleesaves,freshtmps)) (* Restaurar los callee saves *)
       in seq( saveregs @ moveargs @ [body] @ restoreregs ) end
       
fun procEntryExit2(frame:frame,instrs) = instrs (*TODO*)
fun procEntryExit3(frame:frame,instrs) = {prolog = "PROCEDURE ENTRY SCAFFFOLD " ^ #name frame ^ "\n",
                                    body = instrs,
                                    epilog = "PROCEDURE END SCAFFOLD " ^ #name frame ^ "\n"}
end
