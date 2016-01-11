structure tigerflow :> tigerflow =
struct
    open tigergraph
    open tigerassem
    open tigertab
    type graph = tigergraph.graph
    type instr = tigerassem.instr
    type 'a table = 'a tigergraph.mapnode
    type node = tigergraph.node
    type temp = tigertemp.temp

    datatype flowgraph = FGRAPH of {control: graph,
				    def: temp list table,
				    use: temp list table,
				    ismove: bool table}
(*  datatype instr = OPER of {assem:string, src:temp list, dst:temp list, jump:label list option }
               | LABEL of {assem:string, lab:label}
               | MOVE of {assem:string, src:temp, dst:temp}*)



(*
Marga:     datatype flowgraph = FGRAPH of {control: tigergraph.graph,
                                    def: (tigergraph.node * tigertemp.temp list) list,
                                    use: (tigergraph.node * tigertemp.temp list) list,
                                    ismove: (tigergraph.node * bool) list}

no se como crear tablas donde la clave sean de tipo node, creo que el problema esta en que hay un array pero no estoy segura. asi que cambia 	    def: tigertemp.temp list tigergraph.mapnode,
			    use: tigertemp.temp list tigergraph.mapnode,
			    ismove: bool tigergraph.mapnode
a
                def: tigergraph.node * tigertemp.temp list,
			    use: tigergraph.node * tigertemp.temp list,
			    ismove: tigergraph.node * bool
*)  

fun fst (x,_) = x

fun makegraph lInstr = 
        let val gc = newGraph()
            
            fun mktln [] tl tn = (tl,tn) 
              | mktln (LABEL{assem, lab}::inss) tl tn = let val (tl', tn') = mktln inss (tabNueva()) []
                                                            val v = case tn' of
                                                                      [] => raise Fail ("error lab!")
                                                                    | ((n,_)::ns) => n          
                                                            val tn_new = tn@tn'
                                                            val tl'' = tabInserta (lab, v,tl)   
                                                            val tl_new = tabInserList(tl'',(tabAList tl'))
                                                        in (tl_new, tn_new) end       
               |mktln (i::inss) tl tn = mktln inss tl (tn @ [(newNode(gc), i)] )

            val (tablab,tabnode) = mktln lInstr (tabNueva(): (string, node) Tabla) []

            fun mkgr [] d u im = FGRAPH{control = gc, def = d, use = u, ismove = im}
              | mkgr ((n,OPER{src,dst, jump,...})::xs) d u im = let val d' = d @ [(n,src)]
                                                                val u' = u @ [(n, dst)]
                                                                val im' = im @ [(n, false)]         
                                                                val _ = case jump of
                                                                             NONE => if (null xs) then () else mk_edge{from = n, to = fst (List.hd xs)}
                                                                           | SOME lj => List.app (fn j => mk_edge{from = n, to = tabSaca(j,tablab)} ) lj   
                                                            in mkgr xs d' u' im' end
              | mkgr ( (n,MOVE{src,dst, ...})::xs) d u im= let val d' = d @ [(n,[src])]
                                                             val u' = u @ [(n, [dst])]
                                                             val im' = im @ [(n, true)]         
                                                             val _ = if (null xs) then () else mk_edge{from = n, to = fst (List.hd xs)}
                                                       in mkgr xs d' u' im' end
              | mkgr _ _ _ _ = raise Fail ("error mkgr!")
            in (mkgr tabnode [] [] [],List.map (fst) tabnode) end
  (* MakeGraph va a venir acá para no hacer un modulo unitario *)
(*    val makeGraph: instr list -> flowgraph * node list    HACIENDOLA*)

 (*   PASO 1: Agarrar la lista de instrucciones y crear una lista de nodos (sin aristas) y una lista de labels-proxNodo (tabla pa los jumps) ¿De que forma? No se. pre-makeGraph

    PASO 2: Recorrer la lista de nodos(del grafo) y crear las aristas =) (al prox y a los jumps). Por otro lado completar los ismove, los use y los defs. (* Tengo que ir manteniendo el grafo?? *) makeFlowGraph

    fun makeGraph is = 
            let fun makeGraph' is (c, d, u, i) =
                fun makeGraph' [] (g, d, u, i) = (FGRAPH {control = g, def = d, use = u, ismove = i) , nodes g)
                |   makeGraph' (OPER{*)
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
