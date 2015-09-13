Clase 10/9

Codigo para SimpleVar (acceso a una variable)

Este codigo va a funcionar como right value o left value

fun SimpleVar (acc, nivel) = (*nivel de anidamiento, puede estar en otro frame*)
	case acc of
	InReg r => Ex(TEMP r)
	InFrame k => (*k es el offset en el frame*)
		let fun aux 0 = TEMP fp 
			| aux n = MEM(BINOP(PLUS,CONST fpPreLev, aux(n-1)))
		in Ex(MEM(BINOP(PLUS,CONST k,aux(!actualLevel-nivel)))) end

Codigo para r.f:

fun fieldVar(v,f) = 
	let val r = unEx v
            val t = newtemp() (*para evaluar r una sola vez*)
           (*debemos chequear que r no se nil en tiempo de ejecucion*)        
        in 
        ESEQ(ssq[MOVE(TEMP t,r)
                EXP(CALL(NAME "_checkNil"[TEMP t])),], (*las funciones que empizan con _ son de runtime*)
                MEM(BINOP(PLUS,TEMP t, CONST(wSz*f)) (*todos los tipos ocupan lo mismo*)
        end

En el runtime tendremos:

void _checkNil(long sl, long rec) (*el tamaño de long coincide con el del puntero en 16,32 y 64 bits*)
{
	if(rec == 0) assert("record nil!" == NULL);
}

Codigo para exp[i]

fun subscriptVar(v,e) = 
	let val t = newtemp()
	    val ti = newtemp()
	in 
	    ESEQ(seq[MOVE(TEMP t, unEx e), 
		    EXP(CALL(NAME "checkIndex", [TEMP t, TEMP ti])],
		    MEM(BINOP(PLUS,TEMP t, BINOP(MUL, CONST wSz, TEMP ti)))) (*cuando wSz es pot de 2, se puede optimizar SHL, TEMP ti, log wSz*)

Como saber cuanto mide el arreglo?
Durante su creacion guardaremos cuantos elementos tiene.
	 _ ___________
	|T|elemento   |
	  ^
arreglo___| 

En el runtime 

	void _checkIndex(long *arr, long index)
{
	if(arr[-1]<index && index>=0) abort(...);

}	
						
Creacion de un arreglo: A[exp1]  of exp2

fun arrayExp(exp1, exp2)=
	let 
	   val t1 = newtemp()
	   val t2 = newtemp()
	in
	  Ex(ESEQ(Seq[MOVE(TEMP t1, unEx exp1),
			MOVE(TEMP t2, unEx exp2),
			MOVE(TEMP t1, CALL(NAME "_createArray",[TEMP t1, TEMP t2]))],
			TEMP t1))

En el runtime

long _createArray(long ctos, long inicial)
{
	long *ret, int i;
	ret = malloc((ctos+1)*sizeof long);
	ret[0] = ctos;
	for(i=0;i<ctos;i++)
		ret[i+1] = inicial;
	return ret+1;
}

Implementacion de String en Tiger

Posibles implementaciones:

1) C: arreglos de char terminados con NULL('\0')
Ventajas: muy compacto
Desventajas: no puede tener \0

2) Pascal: strings fijas
	
	type S = string(80) (*max 255*) 
	 ___ _________
	|cto|         |

3) Tiger tiene un arreglo a la C con un contador al principio. Similar a los arreglos.

En C definimos:

	typedef struct{
		unsigned long L;
		char S[0];
	}
	
	
	 _ 
	|L|
	  ^
      S___| 

Alojamos asi:

	"hola mundo"
	string *s;
	s = malloc(sizeof(string)+10);
	s->L=10;
	strncpy(s->s,"hola mundo", 10);	

		
				
