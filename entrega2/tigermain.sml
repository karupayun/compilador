open tigerlex
open tigergrm
open tigerescap
open tigercanon
open tigerseman
open tigerinterp
open BasicIO Nonstdio

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (print("Error en parsing!("
	^(makestring(!num_linea))^
	")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")
fun main(args) =
	let	fun arg(l, s) =
			(List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
		val (arbol, l1)		= arg(args, "-arbol")
		val (escapes, l2)	= arg(l1, "-escapes") 
		val (ir, l3)		= arg(l2, "-ir") 
		val (canon, l4)		= arg(l3, "-canon") 
		val (code, l5)		= arg(l4, "-code") 
		val (flow, l6)		= arg(l5, "-flow") 
		val (inter, l7)		= arg(l6, "-inter") 
		val entrada =
			case l7 of
			[n] => ((open_in n)
					handle _ => raise Fail (n^" no existe!"))
			| [] => std_in
			| _ => raise Fail "opcio'n dsconocida!"
		val lexbuf = lexstream entrada
		val expr = prog Tok lexbuf handle _ => errParsing lexbuf
		val _ = findEscape(expr)
		val _ = if arbol then tigerpp.exprAst expr else ()
		
		val _ = transProg(expr)
		
		val _ = print (tigertrans.Ir(tigertrans.getResult()))
		

		val fraglist = tigertrans.getResult() (* fragment list *)
		val canonfraglist = tigercanon.canonize fraglist
		fun insertl e (ls,rs) = (e::ls,rs)
		fun insertr e (ls,rs) = (ls,e::rs)
		fun splitcanon [] = ([],[])
		| splitcanon (x::xs) = 
			case x of
			   (tigerframe.CSTRING s) => insertr s (splitcanon xs)
			   | (tigerframe.CPROC {body,frame}) => insertl (body, frame) (splitcanon xs)
					  
		val (b,c) = splitcanon canonfraglist 
		val _ = tigerinterp.inter inter b c  
		
	in
		print "yes!!\n"
	end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
