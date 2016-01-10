structure tigerflow :> tigerflow =
struct
  
    type graph = tigergraph.graph
    type instr = tigerassem.instr
    type 'a table = 'a tigergraph.mapnode
    type node = tigergraph.node
    type temp = tigertemp.temp

    datatype flowgraph = FGRAPH of {control: graph,
				    def: temp list table,
				    use: temp list table,
				    ismove: bool table}
  datatype instr = OPER of {assem:string, src:temp list, dst:temp list, jump:label list option }
               | LABEL of {assem:string, lab:label}
               | MOVE of {assem:string, src:temp, dst:temp}
  
  (* MakeGraph va a venir acá para no hacer un modulo unitario *)
(*    val makeGraph: instr list -> flowgraph * node list    HACIENDOLA*)

    PASO 1: Agarrar la lista de instrucciones y crear una lista de nodos (sin aristas) y una lista de labels-proxNodo (tabla pa los jumps) ¿De que forma? No se. pre-makeGraph

    PASO 2: Recorrer la lista de nodos(del grafo) y crear las aristas =) (al prox y a los jumps). Por otro lado completar los ismove, los use y los defs. (* Tengo que ir manteniendo el grafo?? *) makeFlowGraph

    fun makeGraph is = 
            let fun makeGraph' is (c, d, u, i) =
                fun makeGraph' [] (g, d, u, i) = (FGRAPH {control = g, def = d, use = u, ismove = i) , nodes g)
                |   makeGraph' (OPER{
  (* Note:  any "use" within the block is assumed to be BEFORE a "def" 
        of the same variable.  If there is a def(x) followed by use(x)
       in the same block, do not mention the use in this data structure,
       mention only the def.

     More generally:
       If there are any nonzero number of defs, mention def(x).
       If there are any nonzero number of uses BEFORE THE FIRST DEF,
           mention use(x).

     For any node in the graph,  
           Graph.Table.look(def,node) = SOME(def-list)
           Graph.Table.look(use,node) = SOME(use-list)
   *)

end
