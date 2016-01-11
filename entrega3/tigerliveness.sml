structure tigerliveness :> tigerliveness =
struct
    open tigergraph
    open tigerflow
    open tigertemp
    open Splaymap
    open Splayset

    datatype igraph = IGRAPH of {graph: graph,
                                 tnode: temp -> node,
                                 gtemp: node -> temp,
                                 moves: (node * node) list}


fun show ig = ()

fun interferenceGraph fg = () (*igraph*(tigergraph.node -> tigertemp.temp list)*)

end
