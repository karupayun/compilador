signature tigerliveness =
sig

    datatype igraph = IGRAPH of {graph: tigergraph.graph,
                                 tnode: tigertemp.temp -> tigergraph.node,
                                 gtemp: tigergraph.node -> tigertemp.temp,
                                 moves: (tigergraph.node * tigergraph.node) list}

    val interferenceGraph : tigerflow.flowgraph -> igraph * (tigergraph.node -> tigertemp.temp list)

(*    val show :(* outstream *) igraph -> unit*)

end

