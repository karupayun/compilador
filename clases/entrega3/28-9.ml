(* 28-9 Selección de instrucciones *)

(* Clase INCOMPLETA *)

Hay, principalmente dos técnicas para la selección de instrucciones.

a) BURS (Bottom-Up Rewriting Systems)
**(Esfuerzo en la generación de reglas del autómata)
El ejemplo más conocido es BURG (B-UP rewriting grammar).

Se puede ver, junto con ejemplos de uso, en A Retargetable C Compiler de Fraser en Hamson.

En esencia son parsers, que toman el lenguaje intermedio (secuencias de tokens de este) y tratan de aplicar reglas de producción. Las gramáticas son ambiguas, pero estas ambiguedades se tratan asignando costos a las reglas de producción; estos costos pueden ser velocidad o espacio. El automata que simula esta gramatica se genera con programación dinámica (Aho y et al.).

Ventajas: Se entiende bastante
Desventaja: Hay que trabajar mucho.

Existe, en SMLofNJ, MLBURG.

b) Maximal Munch. Es simular a BURG, salvo que aquí nosotros somos el autómata. Se basa en ir consumiendo el Tree tratando, en cada caso, de consumir la mayor porción que se pueda. Tanto BURG como MM son optimales, no óptimos. El problema óptimo es NP-completo, y superoptimizer lo demuestra.
**(Esfuerzo en la generación del autómata)

Veamos un ejemplo de cómo hacer esto.

Supongamos que nuestras instrucciones target son (3 operandos).

ADD ri <- rj + rk
MUL ri <- rj * rk
SUB ri <- rj - rk
DIV ri <- rj / rk
ADDI ri <- rj + c (* puede ser al revés *)
SUBI ri <- rj - c
LOAD ri <- Mem [rj+c]
STORE Mem[rj+c] <- ri
MOVEMEM Mem[rj] <- Mem [ri]

Ahora tenemos:

                MOVE
         |-----------------|          
        MEM               MEM
         |                 |
         +                 +
    |--------|          |-----------|
   MEM       *        TEMP fp     CONST x
    |     |---------|
    +   TEMP i    CONST 4
 |----------|
TEMP fp   CONST a

1) 
LOAD r1 <- m[fp+a]
ADDI r2 <- r0 + 4
MUL  r2 <- ri * r2
ADD  r1 <- r1 + r2
LOAD r2 <- mem[fp+x]
STORE Mem[r1+0] <- r2

2) (* Vamos a usar esta *)
LOAD r1 <- Mem [fp+a]
ADDI r2 <- r0 + 4
MUL  r2 <- r1 * r2
ADD  r1 <- r1 + r2
ADDI r2 <- fp + x
MOVEM Mem[r1] <- Mem[r2]

En MM necesitaremos dos funciones: munchExp y munchStm. Así, por ej, en munchStm podemos tener cláusulas como éstas.
open tigertree
fun munchStm (MOVE(MEM(addr as(BINOP(PLUS, e1, CONST i))), e2) = (* M[r1+i] <- e2 *) 
			( munchExp (addr) ; munchExp (e2) ; emit "store")
Esto tiene mucho parecido con una máquina de pila, como la JVM.

	Dirección donde hacer algo
	Valor
	STORE

	| munchStm (MOVE (MEM e1, e2)) = 
		( munchExp e1; munchExp e2; emit "STORE" )


int i = 10;
i += i++; 

i <- i+i
i <- i+1
(* 21 (C, C++, Java nativo) *)

iload i (i) (i = 10)
idup (i,i) (i = 10)
iadd (2*i) (i = 10)
iinc i,1 (2*i) (i = 11)
istore () (i = 20)


(* 22 (Javac, Jikes, gcj a byteorde) *)
