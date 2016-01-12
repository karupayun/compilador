signature tigergraph = 
sig

type graph
type node

val nodes: graph -> node list 
val succ: node -> node list
val pred: node -> node list
val adj: node -> node list
val eq: node*node -> bool
val cmp: node*node -> order (*se necesita esta funcion para poder crear diccionarios cuyas claves sean de tipo node*)

val newGraph: unit -> graph
val newNode: graph -> node
exception GraphEdge
val mk_edge: {from: node, to: node} -> unit
val rm_edge: {from: node, to: node} -> unit

val nodename: node -> string

end
