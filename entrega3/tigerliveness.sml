structure tigerliveness :> tigerliveness =
struct
    open tigergraph
    open tigerflow
    open tigertemp
    open Splaymap
    open Splayset
    
    datatype igraph = IGRAPH of {graph: tigergraph.graph,
                                 tnode: tigertemp.temp -> node,
                                 gtemp: node -> tigertemp.temp,
                                 moves: (node * node) list}
fun error(s) = raise Fail ("Error -- Liveness "^": "^s^"\n")

fun find (s,e) = let val v = Splaymap.peek(s,e)
                 val _ = if (not (isSome v)) then error(nodename e) else ()
                    in (valOf v) end

fun find2 (s,e) = let val v = Splaymap.peek(s,e)
                  val _ = if (not (isSome v)) then error("2") else ()
                     in (valOf v) end


fun printN node gtemp = print (gtemp node^" ")

fun printLN ln gtemp = List.map (fn n => printN n gtemp) ln 

fun show (IGRAPH{graph, tnode, gtemp, moves}) = let val nodes = nodes graph
                                                  val _ = List.map (fn n => (print "\n"; (printN n gtemp); printLN (succ n) gtemp) )  nodes 
                                              in () end   


fun cc (n,s) = (n, List.map makeString s) 

fun listToSetTemp l = Splayset.addList( Splayset.empty cmpt, l) 
(*ecIO calcula pto fijo de las ecuaciones in, out (pag 214)*)
fun ecIO (FGRAPH {control, use, def, ismove}) = let val d = List.foldl (fn (n,d) => Splaymap.insert(d,n, Splayset.empty cmpt)) (Splaymap.mkDict cmp) (nodes control)   
                                        (*eqordl se fija si dos listas ordenadas cuyos elementos son pares de la forma (n,s) son iguales. n es un nodo y s un conj*)
                                         fun eqordl [] [] = true
                                           | eqordl [] _ = false
                                           | eqordl _ [] = false 
                                           | eqordl ((x,s)::xs) ((y,t)::ys) = if (eq(x,y) andalso equal(s,t)) then eqordl xs ys else false   
                                        (*eqd se fija si dos diccionarios son iguales. key = node, a = temp set*)
                                         fun eqd d d' = eqordl (Splaymap.listItems d) (Splaymap.listItems d')
                                        (*pag 214 iteracion*)
                                         fun newIO i out = let val i' = Splaymap.map (fn (n,_) => Splayset.union( difference(find(out,n), listToSetTemp(find( def,n)) ) , listToSetTemp(find(use,n)) ) ) i      
                                                             val o' = Splaymap.map (fn (n,_) => List.foldl (Splayset.union) ( Splayset.empty cmpt) (List.map (fn (n) => find(i,n) ) (succ n)) ) out 
                                                           in (i',o') end
                                        
                                         (*repeti la iteracion si los valores nuevos son distintos a los de antes*)   
                                         fun repeat i out true = (i,out)
                                          | repeat i out false = let val (i',o') = newIO i out
                                                                    in repeat i' o' ( (eqd i i') andalso (eqd out o')) end
                                               

                                    in (repeat d d false) end        
  


fun interferenceGraph (gf as FGRAPH {control, use, def, ismove}) = let val gi = newGraph()
                                                                val (i,out) = ecIO gf 
                                                                val _ = print (Int.toString (length (nodes control)))

 (*newnode': toma un temporario (t), un diccionario q mapea temporarios a nodos (tn)y otro que mapea nodos a temporarios (nt). si el t es una clave de tn devuelve el nodo q le corresponde si no crea un nuevo nodo y lo agrega a ambos diccionarios*)
                                                        fun newnode' t tn nt = case Splaymap.peek(tn,t) of
                                                                                    SOME n => (n,tn,nt)
                                                                                    | NONE => let val n = newNode gi
                                                                                                  val tn' = Splaymap.insert(tn,t,n)
                                                                                                  val nt' = Splaymap.insert(nt,n,t)
                                                                                         in (n,tn',nt') end
                                                        fun addEdge a b tn nt = let val (an, tn',nt') = newnode' a tn nt
                                                                                    val (bn, tn'', nt'') = newnode' b tn' nt'
                                                                                    val _ = mk_edge{from = an, to = bn}
                                                                                    val _ = mk_edge{from = bn, to = an}(*TODO*)
                                                                                in (tn'', nt'') end
                                                        fun addEdges a sb tn nt = Splayset.foldl (fn (b,(tn,nt)) => addEdge a b tn nt) (tn,nt) sb
(*IGNoMove crea los nodos para los temporarios necesarios y agrega las aristas q corresponde (*ver pag 221-222*). IGMove hace lo mismo pero como es un nodo que corresponde a un MOVE se agrega el par (src,dst) a m.*)
                                                      fun iGMove n tn nt m = let val src = List.hd(find(def,n)) (*en los move las listas def y use tienen un solo elemento*)
                                                                                   val dst = List.hd(find(use,n))
                                                                                   val (an,tn',nt') = newnode' src tn nt    
                                                                                   val (cn, tn'', nt'') = newnode' dst tn' nt'
                                                                                   val m' = (an,cn)::m
                                                                                   val b = find(out,n)
                                                                              in (addEdges src (Splayset.difference(b, (listToSetTemp [dst]))) tn'' nt'',m') end
                                                        fun iGNoMove n tn nt = let val a = find(def,n)
                                                                                   val b = find(out,n)
                                                                                in List.foldl (fn (x,(tn,nt)) => addEdges x b tn nt) (tn,nt) a  end
                                                        fun interferenceGraph' nodos tn nt m = List.foldl (fn (n,((tn,nt),m)) => if (find(ismove,n) ) then iGMove n tn nt m else (iGNoMove n tn nt,m) ) ((tn,nt),m) nodos        
 (*igraph*(tigergraph.node -> tigertemp.temp list)*)
                                                        val ((tn,nt),m) = interferenceGraph' (nodes control) (Splaymap.mkDict cmpt) (Splaymap.mkDict cmp) []  
    val igra = (IGRAPH {graph = gi, tnode = fn t => find2(tn,t), gtemp = fn n => find(nt,n), moves = m},fn n => Splayset.listItems(find(out,n)) )          
in show (#1 igra); print "marga"; igra  end


end
