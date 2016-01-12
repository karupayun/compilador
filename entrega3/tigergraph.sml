structure tigergraph :> tigergraph =
struct
  open tigertab
  open Dynarray

  type node' = int
  type temp = tigertemp.temp


  datatype noderep = NODE of {succ: node' list, pred: node' list} 

  val emptyNode = NODE{succ=[],pred=[]}

  val bogusNode = NODE{succ=[~1],pred=[]}

  fun isBogus(NODE{succ= ~1::_,...}) = true
    | isBogus _ = false

  (*structure A = DynamicArrayFn(struct open Array
				    type elem = noderep
				    type vector = noderep vector
				    type array = noderep array
                             end) *)

  type graph = noderep array (* El grafo solo es el array de sucesores y predecesores de cada nodo *)

  type node = graph * node' (* el nodo guarda la información del grafo y un int. Es interesante para no mezclar nodos de grafos distintos. *)
  fun eq((_,a),(_,b)) = a=b (* TEST: Aca falta la comparacion de grafos pero es compleja, no? *)

  fun cmp((_,a),(_,b)) = Int.compare(a,b)

  fun augment (g: graph) (n: node') : node = (g,n)

  fun newGraph() = array(0,bogusNode) (* bogus es decir que no existe *)

  fun nodes g = let val b = bound g
                    fun f i = if isBogus( sub(g,i)) then nil
			           else (g,i)::f(i+1)
		 in f 0			     
                end

  fun succ(g,i) = let val NODE{succ=s,...} = sub(g,i) (* succ solo lleva una lista de enteros, le agrego el grafo *)
		   in map (augment g) s 
		  end
  fun pred(g,i) = let val NODE{pred=p,...} = sub(g,i)
                     in map (augment g) p 
		  end
  fun adj gi = pred gi @ succ gi

  fun newNode g = (* binary search for unused node *) (* Esto imagino que lo hará por si quiere hacer delete más simple *)
    let fun look(lo,hi) =
               (* i < lo indicates i in use
                  i >= hi indicates i not in use *)
            if lo=hi then (update(g,lo,emptyNode); (g,lo))
            else let val m = (lo+hi) div 2
                  in if isBogus(sub(g,m)) then look(lo,m) else look(m+1,hi)
                 end
     in look(0, 1 + bound g)
    end

  exception GraphEdge
  fun check(g,g') = (* if g=g' then () else raise GraphEdge *) () (* Posible modificacion para comparar grafos. Necesitaríamos ponerle direcciones. *) 

  fun delete(i,j::rest) = if i=j then rest else j::delete(i,rest)
    | delete(_,nil) = raise GraphEdge

  fun diddle_edge change {from=(g:graph, i),to=(g':graph, j)} = (* Formato general para operaciones sobre aristas *)
      let val _ = check(g,g')
          val NODE{succ=si,pred=pi} = sub(g,i)
          val _ = update(g,i,NODE{succ=change(j,si),pred=pi})
          val NODE{succ=sj,pred=pj} = sub(g,j)
          val _ = update(g,j,NODE{succ=sj,pred=change(i,pj)})
       in ()
      end

  val mk_edge = diddle_edge (op ::) (* Aplica la operación :: a la arista *)
  val rm_edge = diddle_edge delete  (* Aplica la operación delete a la arista *)

  (*structure Table = IntMapTable(type key = node
				fun getInt(g,n) = n)*)


  fun nodename(g,i:int) = "n" ^ Int.toString(i) (* Para debug *)

end

