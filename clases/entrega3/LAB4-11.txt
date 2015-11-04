(* Etapa 3: BackEnd *)
---------------     |-------------------|       -----------         -------------------
| CANONIZADOR | --> | SELECCION DE INST | ->(*) | COLOREO | -> (**) | ESCRITURA Y GCC |
---------------     |-------------------|       -----------         -------------------

* Por cada fragmento de función, una lista de assem.instr
fun assem.format: (Temp.temp -> string) -> assem.instr -> string

Página de appel:
    www.cs.princeton.edu/~appel/modern/ml/project.html -> link a assem.sml
    
** Ustedes deciden:
    --simple reg alloc: pero cada lista de assem.instr, una lista de assem.instr donde los src y dst son registros reales.
    - devolver sólo la tabla de coloreo (más lo anterior)
    
    
    Después del coloreo tendríamos
    
   a) Una lista de assem.instr por cada función, más una tabla que asocia registros ficticios a registros reales.
   (Con estas dos cosas y assem.format obtenemos el assembler de cada función, como un string).
   b) Un frame.frame por cada función. Sirve para ver cuanto stack usa cada una.
   c) Fragmentos STRING.
   
   
Con (b) se puede hacer el prólogo y el epílogo de cada función.

Prólogo: 
".globl func
.type func,@function
func:
    pushq   %rbp
    movq    %rsp,   %rbp
    subq    $xxx,   %rsp"
    
    
Epílogo:
"movq   %rbp,   %rsp    
 popq   %rbp
 ret"
 
 Esto lo hace procEntryExit3 (pag 243)
 
 Con (c) hay que pasar a assembler
    Ej: "Hola", label L
        "L:
            .long 4
            .ascii \"Hola\""
            
            
".section rodata"
    ...
    ...
    Los strings
    ...
    ...
".text"
    ...
    ...
    Las funciones
    ...
    ...   
            
            
val out = TextIO.openOut "xxx.s"
val _ = TextIO.output (out,"Hola Mundo")
val _ = TextIO.closeOt out

val _ = process.system ("gcc xxx.s runtime.o")

