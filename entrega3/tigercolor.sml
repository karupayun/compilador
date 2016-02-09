structure tigercolor :> tigercolor =
struct
    
    open tigerframe
    open Splayset
    open tigertemp

  	type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict (* Cada temp a su registro *)


    val precolored = addList ((empty tigertemp.cmpt), []) (*TODO [] tigerframe*)
    val k = numItems precolored

	fun addSet (s,e) = s := add (!s,e)
	fun hayElem s = not(isEmpty(!s))
	fun deleteSet (s,e) = if member(!s,e) then s:= delete (!s,e) else ()
	fun pertSet (s,e) = member(!s,e)
    fun takeSet s = hd(listItems(!s))    
	fun takeSetP (s,e) = e := hd(listItems(!s))
	fun vaciar (s,cmp) = s := empty cmp  
	fun toSet (s,cmp) = addList  (empty cmp,!s) 

    fun isEmptyStack p = case !p of 
                        [] => true
                        | _ => false

	fun push (p,e) = p := (!p @ [e])
    fun pop p = let val e = case !p of 
                             [] => raise Fail ("error pop")
                            | (x::xs) => (p:= xs; x)
                    in e end 
	fun popP (p,e) = (e := hd(!p) ; p := tl(!p))


    val simplifyWorklist = ref (empty tigertemp.cmpt ) 
    val freezeWorklist = ref (empty tigertemp.cmpt)
    val spillWorklist = ref (empty tigertemp.cmpt)

    val spilledNodes = ref (empty tigertemp.cmpt)
    val coalescedNodes = ref (empty tigertemp.cmpt)
    val coloredNodes = ref (empty tigertemp.cmpt)
    val selectStack = ref ([] : (tigertemp.temp list))

    val coalescedMoves = ref (empty tigergraph.cmp) 
    val constrainedMoves = ref (empty tigergraph.cmp)
    val frozenMoves = ref (empty tigergraph.cmp)
    val worklistMoves = ref (empty tigergraph.cmp)
    val activeMoves = ref (empty tigergraph.cmp)

    fun cmpTT ((t1,t2),(t1',t2')) = case tigertemp.cmpt(t1,t1') of
                                      EQUAL => tigertemp.cmpt(t2,t2')
                                    | c => c


    val adjSet = ref (empty cmpTT)
    val adjList = ref (Splaymap.mkDict tigertemp.cmpt) 
    val degree = ref (Splaymap.mkDict tigertemp.cmpt)
    val moveList = ref (Splaymap.mkDict tigertemp.cmpt)
    val alias = ref (Splaymap.mkDict tigertemp.cmpt)
    val color = ref (Splaymap.mkDict tigertemp.cmpt)

    val initial = ref (empty tigertemp.cmpt)
    
    val (fgraph as tigerflow.FGRAPH{ismove,def,use,...},nodos) = tigerflow.makeGraph [] (*TODO instr list: arg*)
    val (igraph,liveOut) = tigerliveness.interferenceGraph fgraph

fun getDict(d,k,v) =  Option.getOpt (Splaymap.peek(!d,k),v)
fun getDegree k = getDict(degree,k,0)
                    
fun addDict(d,k,v) = d := Splaymap.insert(!d,k,v) 
(*fun modifDict(d,f) = d:= Splaymap.map(f,!d)*)

fun addEdge u v = 
    if not(pertSet(adjSet, (u,v))) andalso (u<>v) then (
        addSet(adjSet, (u,v)); 
        addSet(adjSet, (v,u)); 
        if not(member(precolored,u)) then (  
            addDict(adjList,u, add(getDict(adjList,u,empty tigertemp.cmpt), v) );
            addDict(degree,u,(getDegree u)+1) ) 
        else ();  
        if not(member(precolored,v)) then (
            addDict(adjList,v, add(getDict(adjList,v,empty tigertemp.cmpt),u) );
            addDict(degree,v,(getDegree v)+1) )
        else () )  
    else ()

fun build() = List.app (fn i => let val live = ref (addList (empty tigertemp.cmpt,(liveOut i)))
                                    val ismoveI = Splaymap.find(ismove ,i)
                                    val useI = addList (empty tigertemp.cmpt, Splaymap.find(use,i) )
                                    val defI = addList (empty tigertemp.cmpt,Splaymap.find(def,i)  )      
                                in if ismoveI then
                                    (live := difference(!live, useI);
                                    app (fn n => addDict(moveList,n, add(getDict(moveList,n,empty tigergraph.cmp),i))) (union(useI,defI)) ;
                                    addSet(worklistMoves,i) ) else ();
                                    live := union(!live,defI);
                                    app (fn d => app (fn l => addEdge l d) (!live) ) defI;
                                    live := union(useI,difference(!live,defI)) end ) (rev nodos)
                                                            
                                
fun nodeMoves n = intersection(getDict(moveList,n,empty tigergraph.cmp), union(!activeMoves, !worklistMoves)) 

fun moveRelated n = not( isEmpty(nodeMoves n)) 

fun makeWorkList() = ( app (fn n => if (getDegree n) >= k then addSet(spillWorklist,n) else if (moveRelated n) then addSet(freezeWorklist,n) else addSet(simplifyWorklist,n)  ) (!initial); vaciar(initial, tigertemp.cmpt) )
    

fun adjacent n = difference(getDict(adjList,n,empty tigertemp.cmpt), union(toSet(selectStack,tigertemp.cmpt), !coalescedNodes))

fun enableMoves nodes = let fun aux m = if pertSet(activeMoves,m) then (deleteSet(activeMoves,m); addSet(worklistMoves,m)) else ()
                        in app (fn n => app (aux) (nodeMoves n)) nodes end

fun decrementDegree n = let val d = getDegree n
                        in  addDict(degree, n, d-1 );
                            if d = k then (
                                enableMoves (add(adjacent n, n));
                                deleteSet(spillWorklist,n);
                                if (moveRelated n) then addSet(freezeWorklist, n) else addSet(simplifyWorklist,n) )
                            else () end
                         
fun simplify() = let val n = takeSet(simplifyWorklist)
               in deleteSet(simplifyWorklist,n);
                  push(selectStack, n);
                  app (decrementDegree) (adjacent n) end


fun addWorkList u = if not(member(precolored,u)) andalso not(moveRelated u) andalso (getDegree u) < k then (deleteSet(freezeWorklist,u); addSet(simplifyWorklist,u) )else ()

fun ok(t,r) = (getDegree t) < k orelse member(precolored,t) orelse pertSet(adjSet,(t,r)) (*George*)

fun conservative nodes = (foldl (fn (n,b) => if ( (getDegree n) >= k) then b+1 else b)  0 nodes) < k (*Briggs*)
                                
fun getAlias n = if pertSet(coalescedNodes,n) then getAlias(Splaymap.find(!alias,n)) else n

fun combine (u,v) =( if pertSet(freezeWorklist,v) then deleteSet(freezeWorklist,v) else deleteSet(spillWorklist,v); 
                     addSet(coalescedNodes,v);
                     addDict(alias,v,u);
(*cambie nodeMoves(func) por moveList(dict)-- ver pag 259!!*)                     addDict(moveList,u, union(getDict(moveList,u,empty tigergraph.cmp),getDict(moveList,v,empty tigergraph.cmp)));
                     app (fn t => (addEdge t u; decrementDegree t)) (adjacent v);
                    if (getDegree u) >= k andalso pertSet(freezeWorklist,u) then (deleteSet(freezeWorklist,u);addSet(spillWorklist,u) ) else () )

 
fun src m = hd (Splaymap.find(use,m) )

fun dst m = hd ( Splaymap.find(def,m) )

fun coalesce() = let val m = takeSet(worklistMoves)
                     val (x,y) = (getAlias (src m), getAlias (dst m)) 
                     val (u,v) = if member(precolored, y) then (y,x) else (x,y)
                   in deleteSet(worklistMoves, m);
                       if (u = v) then
                          (addSet(coalescedMoves,m); 
                          addWorkList u) 
                       else if member(precolored,v) orelse pertSet(adjSet,(u,v)) then 
                               (addSet(constrainedMoves,m); 
                               addWorkList u;
                               addWorkList v) 
                       else if (member(precolored,u) andalso (foldl (fn (t,b) => b andalso ok(t,u)) true  (adjacent v) )) orelse (not(member(precolored,u)) andalso conservative (union ( adjacent u,adjacent v ))) then 
                                (addSet(coalescedMoves,m); 
                                combine(u,v); 
                                addWorkList u)
                       else addSet(activeMoves, m) end
                            
fun freezeMoves u = app (fn m => let val (x,y) = (src m,dst m) 
                                     val v = if (getAlias y = getAlias u) then getAlias x else getAlias y
                                     in deleteSet(activeMoves,m);
                                        addSet(frozenMoves,m);
                                        if isEmpty(nodeMoves v) andalso getDegree v < k then 
                                           ( deleteSet(freezeWorklist,v);
                                            addSet(simplifyWorklist,v) )
                                        else () end ) (nodeMoves u)

fun freeze() = let val u = takeSet(freezeWorklist)
                 in deleteSet(freezeWorklist,u); addSet(simplifyWorklist,u); freezeMoves u end

fun spillCost n = 0 (* TODO *) (* Esta función debería pasarse como argumento según el sig, aunque se podría calcular acá mismo. *)
(* SpillCost:
Sacado de internet:
Spilling:
What should we spill?
– Something that will eliminate a lot of interference edges
– Something that is used infrequently
– Maybe something that is live across a lot of calls?
•
Giving up too quickly
One Heuristic:
– spill cheapest live range (aka “web”)
– Cost = [(# defs & uses)*10 loop-nest-depth ]/degree

Pablo: Por simplicidad se podría usar algo que minimize los usos/def y/o maximize el grado.

* Selectspill va a elegir el spill del mínimo costo.

)*

*)

fun selectSpill () = let val m = foldl (fn (i,ac) => (if spillCost(i) < spillCost(ac) then i else ac)) (takeSet (spillWorklist)) (!spillWorklist)
					 in deleteSet (spillWorklist, m); addSet(simplifyWorklist,m); freezeMoves m end


fun spillear () = ref (empty tigertemp.cmpt) (* TODO *) (* HACER Algoritmo de Marian para spillear nodos de SpilledNodes. 
							- Allocar memoria para cada nodo de SpilledNodes
							- Crear nuevo temporario vi para cada definición y cada uso
							- Insertar un "store" antes de cada definicion y un "carga" antes de cada uso.
							- Poner todos los vi en un conjunto de "nuevos temporarios"		
							- Devolver esos temporarios*)

fun rewriteProgram () = let val newTemps = spillear()
						in vaciar (spilledNodes,tigertemp.cmpt); initial := union (!coloredNodes, (union (!coalescedNodes, !newTemps))); vaciar (coloredNodes,tigertemp.cmpt); vaciar (coalescedNodes,tigertemp.cmpt)  end

fun main () = () (* TODO *)
              	
fun assignColor() = let fun colorea n = let val okColors = ref (empty Int.compare) (*TODO lista de 0 a k-1*)
                                        in app (fn w => if member(union(!coloredNodes, precolored), getAlias w) then deleteSet(okColors, getDict(color,getAlias w, k+1)) else () ) (getDict(adjList,n,empty cmpt)) ;
                                           if not (hayElem okColors) then addSet(spilledNodes,n) else (addSet(coloredNodes,n); addDict (color,n, takeSet okColors))
                                        end
                        fun repeat () = if not(isEmptyStack selectStack) then (colorea (pop selectStack); repeat ()) else ()
                        in repeat(); app (fn n => addDict(color,n,getDict(color,getAlias n, 0)) ) (!coalescedNodes) end
end
