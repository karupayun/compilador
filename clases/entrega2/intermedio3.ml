(* Código Intermedio -- Clase 3 -- 7/9*)

Mas codigo intermedio

Definimos el lenguaje tree

struct tigertree =
struct
	type label = tigertemp.label
	datatype exp = CONST of int
			| NAME of label
			| TEMP of tigertemp.temp
			| BINOP of binop*exp*exp
			| MEM of exp 
			| CALL of exp* exp list
			| ESEQ of stm * exp
	and	stm = MOVE of exp*exp
			|EXP of exp
			| JUMP of exp * label list (*goto multiple*)
			| CJUMP of relop * exp*exp*label*label
			| SEQ of stm *stm
			| LABEL of label
	and relop = EQ|NE|LT|GT|LE|GE|ULT|UGT|ULE|UGE
	and binop = PLUS | MINUS | MUL | DIV | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

Ahora, en tigerframe, agregamos

datatype frag = PROG of {body: tigertree.stm, frame:frame}
		| STRING of tigertemp.label * string(*para las strings constantes*)

y comenzaremos el modulo de traduccion a tree

struct tigertrans:>tigertrans = 
struct 
	datatype exp = SCAF | Ex of tigertree.exp | Nx(no es una expresion) of tigertree.stm| Cx(expresion condicional) of tigertemp.label*tigertemp.label-> tigertree.stm

Este ultimo constructor recibe una funcion que toma dos etiquetas y genera la condicion. Esto viene bien, pues la condicion 
			a<b | b>c
tendra efectos distintos si aparece, por ejemplo en 
	if a>b|b>c then... (aca tiene que generar un salto)
o 
	x:= a>b|b>c (aca tiene que generar un 1 o 0)

La generacion de tree sigue (mas o menos) los lineamientos de las, asi llamadas, semanticas de punto fijo o denotacionales. Hay un pequeño inconveniete, que es que tree va aser empaqueado para ser devuelto a tigerseman y debe ser desempaquetado al vovler a tigertrans.

(*desempaquetadoras*)

fun unEx(Ex e) = e
| unEx(Cx gs) = (*tiene que devolver un 1 o un 0*)
	let val (r,t,f) = (newtemp(),newlabel(),newlabel())
	in 
		ESEQ(seq[MOVE(TEMP r,CONST 1),
			gs(t,f), (*chequea la condicion *)
			LABEL f, MOVE(TEMP r,CONST 0),
			LABEL t], TEMP r)
	end
| unEx(Nx s) = ESEQ(s, CONST 0)

fun unNx(Ex e) = EXP e
| unNx(Nx s) = s
| unNx(Cx c) = 
	let val l = newlabel()
	in seq[C(l,l),LABEL l]
	end 
fun unCx (Cx c) = c
| unCx(Nx n) = raise Fail "error interno #@@"
| unCx(Ex e) = fn(t,f) => 
		CJUMP(NE,CONST 0,e,t,f)

fun seq [] = EXP (CONST 0)
| seq [e] = e
| seq (h::s)  SEQ(h, seq s)

En tigerseman teniamos algo como 

trexp(UnitExp, _ )= {exp = SCAF, ty = TUnit}

Ahora lo cambiamos por
trexp(UnitExp, _) = {ty=TUnit, exp = unitExp()}

y, en tigertrans,
fun unitExp () = Ex(CONST 0)
fun nilExp () = Ex(CONST 0) 
fun intExp n = Ex(CONST n)

por supuesto, en seman, 
	|trexp (IntExp(n,_)) = {ty= TInt RW, exp = intExp n}

En seman, 
	|trexp (IfExp({test, then', else'=NONE},nl)) = 
	let val {exp = te', ty= tt'} = trexp test
	    val {exp = the', ty = thte'} = trexp then'
	    val _ = esInt(tt',tenv, nl)
	    val _ = if thte' <> TUnit then error("no es unit") else ()
	in 
		{exp = ifThenExp{test=te', then'=the'},ty = TUnit}
	end
En trans
	fun ifThenExp{test, then'} = 
		let val cj = unCx test(*lo desempaquetamos como una condicion*)
		    val(l1,l2) = (newlabel(), newlabel())
		in 
		   Nx(seq[cj(l1,l2),LABEL l1,unNx then', LABEL l2]) (*if sin else no da ningun valor*)
		end 		

fun ifThenElseExp{test, then',else'} = 
		let val cj = unCx test(*lo desempaquetamos como una condicion*)
		    val(l1,l2,l3) = (newlabel(), newlabel(),newlabel())
		    val temp = newtemp()			
		in 
		   Ex(ESEQ(seq[
			cj(l1,l2),
			LABEL l1, SEQ(MOVE(TEMP tmp, unEx then')),JUMP(LABEL l3,[l3])) 
			LABEL l2, SEQ(MOVE(TEMP tmp, unEx else')),
			LABEL l3)],
			TEMP tmp))
		end

fun whileExp{test, body}=
	let val (l1,l2,sal) = (mewlabel(),newlabel(), topsalida())
	in Nx(seq[JUMP(NAME l1,[l1]),
	          LABEL l2, unNx body, 
		  LABEL l1, (unCx test) (l2,sal),
		  LABEL sal])	
	end

Para lidiar con los break, definimos en trans 
fun preWhileForExp () = 
	pushSalida(SOME(newlabel())) 

fun postWhileForExp = 
	popSalida() 

En seman,
| trexp(WhileExp({test,body}), nl) = 
	let val {exp = te', ty = tt'} = trexp test
	    val _ = preWhileForExp () (*la llamamos despues de hacer trexp test para que esto ande bien: while (break;1) do ()*)
	    val {exp = be', ty = bt'} = trexp body
	chequeos
	val ev' = whileExp{te',be'}
	val _ = postWhileForExp()
	in 	
		{exp=ev',ty=TUnit}

Ahora BreakExp es trivial
	| trexp(BreakExp nl) = 
	(({exp = BreakExp(), ty=TUnit})
		handle Fail s => raise Fail(s^NL nl))	
En trans
	fun breakExp()=
		let val l = topsalida()
		in 
			Nx(JUMP(NAME l,[l]))
		end

EJ:
(if 3>4 then 5 else 4)	da este codigo tree:
	
-tigermain:
	SEQ(EXP(CONST 0), MOVE(TEMP rv, 
	ESEQ(SEQ(CJUMP(GT,CONST 3,CONST 4,.l0,.l1),
	SEQ(LABEL .l0, SEQ(SEQ(MOVE(TEMP T0,CONST 5), 
	JUMP(NAME.l2,[.l2])),
	SEQ(LABEL .l1,SEQ(MOVE(TEMP T0, CONST 6),
	LABEL .l2))))), 
	TEMP T0)))

Veamos el for con mas cuidado...
Es una tentacion considerar esta equivalencia 
	for i:=lo to hi do body 
		===
	i:=lo;
	while not(i>hi) do(body; i:=i+1)
que es falsa. No funciona si hi = entero maximo, el while no termina nunca pero el for si debe terminar.

fun forExp{var, lo, hi, body} = (*hay que evaluar siempre hi y lo por si producen efecto laterales*) 
	let val var' = unEx val
	let val (l1,l2,sal) = (newlabel(),newlabel(), topsalida())
	in Nx(seq(case hi of 
			Ex(CONST n) => 
				if n<valOf(Int.minInt) then [MOVE(var', unEx lo),
							     JUMP(NAME l2, [l2]),
							     LABEL l1, unNx body,
							   MOVE(var', BINOP(PLUS, var',CONST 1)),
								LABEL l2, CJUMP(GT,VAR',CONST n, sal, l1),
								LABEL sal]
			       else [MOVE(var', unEx lo),
				   LABEL l2, CJUMP(GT,VAR',CONST n, sal, l1),
				   unEx body, LABEL l1,MOVE(var', BINOP(PLUS, var',CONST 1)), 
				   JUMP(NAME l2, [l2]),	
				   LABEL sal]
	
			| - => let val t = newtemp ()
			
		              in [MOVE(var', unEx lo), 
				  MOVE(TEMP t, unEx hi), (***)	
				  CJUMP(LE(var',TEMP t, l2,sal),
				  LABEL l2, unNx body,
				  CJUMP(EQ, TEMP t, var', sal, l1),
				  LABEL l1,MOVE(var', BINOP(PLUS, var',CONST 1)), 
				   JUMP(NAME l2, [l2]),	
				   LABEL sal]			
			end))
	end

(* Este último caso evita el problema de:
	h:= 10;
	for i:=1 to h do(
		print (chr (65+h);
		h:=h-1; (* ya esta guardado en (***) *)
	)

 *)

Con vistas a encarar el codigo para CallExp, veamos cómo manejar el STATIC LINK. El SL es un puntero al ultimo frame dinamico de la ultima funcion anidante:
		let 					stack
			function f() =              	|_|
			 let function g() =  --- fp---->|f| último 
				body		     |  |_|
			in g() end		     |	|.|
						     |	|.|
						     |	|_|
			  	       (static link) ---|g|
							|_|
		...

1er Caso: prof(g) > prof (f) => prof (g) = prof(f)+1
	SL(g) = FP(f)
2do Caso: prof(g) = prof (f) => SL (g) = SL (f)
3er Caso: prof(g) < prof (f) => g está llamando a sus anidantes =>
		temp t <- SL (f)
	iterar	prof(f) - prof(g) veces
			temp t <- MEM (temp t + offset SL)  
		SL (G) = temp t
		
