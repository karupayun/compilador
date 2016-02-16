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
		
		(* val _ = print (tigertrans.Ir(tigertrans.getResult())) *) (* imprime el tree*)
		val fraglist = tigertrans.getResult() (* fragment list *)
		val (stringlist,funclist) = canonize fraglist
		(*val _ = tigerinterp.inter inter b c  (* ARREGLAR *)*)
        (* val _ = List.app (fn(stms,frame) => print(tigertrans.Ir( [tigerframe.PROC{body=tigerframe.seq stms,frame=frame}] )) ) funclist (* imprime el resultado del canon *) *)
        (* val _ = List.app ( fn(s,l) => print(tigertemp.makeString l^": "^s^"\n") ) stringlist *)
        fun procFunc (stms,frame) = let val instrs = tigercodegen.codegens stms
                                        val instrsEE2 = tigerframe.procEntryExit2(frame,instrs)
                                        val {prolog,epilog,body=instrsEE3} = tigerframe.procEntryExit3(frame,instrsEE2)
                                        val _ = print("TRY\n")
                                        val (instrColored, dictAlloc) = tigercolor.alloc(instrsEE3,frame)
                                        val _ = print("END\n")
                                        fun saytemp t = Option.getOpt(Splaymap.peek(dictAlloc,t), t)
                                        val strListBody = List.map (tigerassem.format saytemp) instrColored
                                        val strBody = List.foldr (fn(x,e)=>x^"\n"^e) "" strListBody
                                    in print(prolog ^"\n"^ strBody ^"\n"^ epilog^"\n") end

        (* val afunclist = List.map ( fn (stms,frame) => (tigercodegen.codegens stms,frame) ) funclist
        val afunclistproc = List.map ( fn (instrs,frame) =>  tigerframe.procEntryExit3(frame,tigerframe.procEntryExit2(frame,instrs) ) ) afunclist (*OBSOLETO*) *)
        (* val printinstr = tigerassem.format (fn x => x)
        fun printbody instrs = List.foldr (fn(a,b)=>a^"\n"^b) "" (List.map printinstr instrs)
        val _ = List.app ( fn {prolog, body, epilog} => print(prolog ^ (printbody body) ^ epilog) ) afunclistproc *)
        val _ = List.app procFunc funclist
	in
		print "yes!!\n"
	end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
