structure tigertemp :> tigertemp =
struct
        type temp = string
        type label = string
        local
            val actTemp = ref 0
            val actLabel = ref 0
        in
            fun newtemp() = ("T"^(!actTemp)) before actTemp := !actTemp+1
            fun newlabel() = ("L"^(!actLabel)) before actLabel := !actTemp+1
        end
        
(* makeString hace la representación de una StringExp como una etiqueta *)

fun makeString temp =
              let val largo = Int.toString(String.size (temp + 1))
              in "\n\n.long"^largo^"\""^temp^"\"" end
(* sintaxis de gas. Si usan otra cosa adecúen esto *)

fun namedlabel s = s
fun string2temp s = s:temp
etc.
end

Parte de la info del frame será para alocar variables de la función.
Las variables que puede acceder el código de una función será
  - las locales definidas con var (son alocadas al entrar a la función)
  - los argumentos (son alocados por la llamante. Pero debemos ver dónde estarán)
  - las variables de las funcs. anidantes (alocadas por las anidantes. Debemos saber cómo acceder al frame donde están).
  
Sin frames dinámicos perdemos recursividad
Los frames dinámicos se pueden gestionar de varias maneras.
  -Manipularlas mediante un stack (Lifo)
  -Crearlos en el heap y hacer garbage collection (SML o FN)
  
Nosotros si es posible, usaremos un stack. Con esto, un marco de activación tendrá esta pinta:

        |_________________|
        |                 | } argumentos según la 
        |                 | }
        |_________________| } convención de llamada y/o escapados
        |     retorno     | 
        |_________________|
fp(frame| registros       |
pointer)| calee-saves     |
        |_________________|
        |   fp -n.w       |
        |_________________|
        |   fp -(n n).w   | } Locales escapadas
        |_________________| }
        |                 |
SP ---> |-----------------|

Respetaremos la convención de llamada para poder linkear con C

structure tigerframe :> tigerframe =
struct
    open tigertemp
    open tigertree (*no lo vimos aún *)
    type level = int
    (* temporarios predefinidos. Tomar esto como ejemplo *)
    val fp = string2temp "FP" (* frame pointer *)
    val sp = string2temp "SP" (* stack pointer *)
    val rv = string2temp "RV" (* return value *)
    val ov = string2temp "OV" (* overflow p/cociente, edx en i386 *)
    val wSz = 8 (* word size, este valor es para x86_64 *)
    val logwSz = 3 (* log2 wSz *)
    (* para manejar los argumentos *)
    val argInicial = 1
    val argDelta = 1 (* 1 palabra *)
    
Nota: toddas las funciones Tiger recibirán un arg implícito
Esto para resolver el problema del acceso a las variables escapadas.
El escenario es
    f anida a g anida a h
Para solucionar este problema existen dos técnicas

-Tabla Display (Dijkstra)
  Si una función tiene un nivel de anidamiento n construye un arreglo de n punteros que apuntan a los frame de los anidantes.
  Se puede armar recursivamente.
  
  Ventaja : acceso de timepo constante
  Desventaja: construcción lineal
  
  -Lambda lifting
  Ejemplo tomado de oo2c (object oberon to C)
  
    MODULE m:
      FUNCTION f()
        VAR x:INTEGER
          FUNCTION g()
            VAR y:INTEGER
            FUNCTION h()
            BEGIN
              RETURN x+y;
            END h;
          BEGIN
            y:=10;
            h();
          END g;
        BEGIN
          x:=11;
          g();
        END f
      BEGIN
        f()
      END
      
Su traducción en C tiene esta pinta

int h_3(int *x,int *y)
{
    return *x + *y;
}

void g_2(int *x)
{
    int y;
    y = 10;
    h_3(x,&y);
}

void f_1()
{
    int x;
    x:=11;
    g_2(&x);
}

int main()
{
  f_1()
  return 0;
}

Así la esencia del lambda lifting es agregar como argumentos, punteros a las variables libres (que deben ser locales de las anidates).
  Ventaja: funciona
  Desventaja : posible explosión de variables
-Una lista enlazadas
Toda función recibe un puntero a su última anidante (static link o sl)
lo único que hay que cuidar es cómo generar el SL

Volvamos al frame
Definimos
  datatype access = InReg of temp
                | InFrame of offset (* respecto al fp *)
  type frame = {
            name : string,
            formals : bool list, (* si son escapadas *)
            locals : bool list,
            actualArg : int ref, (* último arg generado *)
            actualLocal :int ref, (* último local generado *)
            actualArgReg: int ref
