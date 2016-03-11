structure tigercolor :> tigercolor =
struct
    
    open tigerframe
    open Splayset
    open tigertemp
    open tigerassem
  	type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict (* Cada temp a su registro *)

fun alloc (instrs, frame_arg) = let

 (*   fun printTemps t = map (fn x => (" "^x)) t
    fun printIns (OPER {assem, src, dst, jump}) = print ("OPER "^assem^ printTemps src ^ printTemps dst ^"\n")  
    |   printIns (LABEL {assem, lab}) = print ("LABEL "^assem^"\n")
    |   printIns (MOVE {assem, src, dst}) = print ("MOVE "^assem^ src ^" "^ dst^"\n")
*)
    val _ = print("test1\n")
    val precolored = addList ((empty tigertemp.cmpt), tigerframe.coloredregisters) 
    val notcolored = addList ((empty tigertemp.cmpt), tigerframe.specialregs)
    val k = numItems precolored
    fun error(s) = raise Fail ("Error -- Find "^": "^s^"\n")

    fun find (s,e) = let val v = Splaymap.peek(s,e)
                     val _ = if (not (isSome v)) then error("q") else ()
                       in (valOf v) end

	fun addSet (s,e) = s := add (!s,e)
	fun hayElem s = not(isEmpty(!s))
	fun deleteSet (s,e) = if member(!s,e) then s:= delete (!s,e) else ()
	fun pertSet (s,e) = member(!s,e)
    fun takeSet s = hd(listItems(!s))    
	fun takeSetP (s,e) = e := hd(listItems(!s))
	fun vaciar (s,cmp) = s := empty cmp  
	fun toSet (s,cmp) = addList  (empty cmp,!s) 
    fun toList s = listItems (!s)
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
	
    val _ = print("test10\n")

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
    val countUsesDefs = ref (Splaymap.mkDict tigertemp.cmpt)
    val moveList = ref (Splaymap.mkDict tigertemp.cmpt)
    val alias = ref (Splaymap.mkDict tigertemp.cmpt)
    val color = ref (Splaymap.mkDict tigertemp.cmpt)

    val _ = print("test20\n")
    
    val lInstr = ref (instrs)
    val _ = print("test21\n")
    val fg_nodos = ref (tigerflow.makeGraph (!lInstr)) (* Le dejo eso para calcular initial *)
    val _ = print("test22\n")    
    val ig_liveOut = ref (tigerliveness.interferenceGraph (#1 (!fg_nodos)))
    val _ = print("test23\n")
    val initial = let val tigerliveness.IGRAPH{graph,gtemp,...} = (#1 (!ig_liveOut))
                        in print("test24\n");ref (difference((addList (empty tigertemp.cmpt, List.map (gtemp) (tigergraph.nodes graph))), union(precolored, notcolored))) end (*DUDA: Llene esto, porque me parecía que era cualca que esté vacío. Quizá hay una forma más fácil. Cualquier cosa avisen.*) (*Le reste los precoloreados.. pag 253*)
    val _ = print ("initial\n")
    val _ = Splayset.app (fn x => print(x^" ") ) (!initial)
    val _ = print ("\n")
    



    val _ = print("test28\n")
    val frame = frame_arg

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

    fun build() = let val tigerflow.FGRAPH{ismove,use,def,...} = (#1 (!fg_nodos))
                   in List.app (fn i => let val live = ref (addList (empty tigertemp.cmpt,((#2 (!ig_liveOut)) i)))
                                        val ismoveI = find(ismove ,i)
                                        val useL = find(use,i)
                                        val defL = find(def,i)
                                        val useI = addList (empty tigertemp.cmpt, useL )
                                        val defI = addList (empty tigertemp.cmpt, defL )   
                                    in if ismoveI then
                                        (live := difference(!live, useI);
                                        app (fn n => addDict(moveList,n, add(getDict(moveList,n,empty tigergraph.cmp),i))) (union(useI,defI)) ;
                                        addSet(worklistMoves,i) ) else ();
                                        List.app (fn i => addDict (countUsesDefs, i, getDict(countUsesDefs, i, 0) + 1)) (useL);
                                        List.app (fn i => addDict (countUsesDefs, i, getDict(countUsesDefs, i, 0) + 1)) (defL);
                                        live := union(!live,defI);
                                        app (fn d => app (fn l => addEdge l d) (!live) ) defI;
                                        live := union(useI,difference(!live,defI)) end ) (rev (#2 (!fg_nodos)))
                    end                                            

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

        val _ = print("test30\n")
    fun addWorkList u = if not(member(precolored,u)) andalso not(moveRelated u) andalso (getDegree u) < k then (deleteSet(freezeWorklist,u); addSet(simplifyWorklist,u) )else ()

    fun ok(t,r) = (getDegree t) < k orelse member(precolored,t) orelse pertSet(adjSet,(t,r)) (*George*)

    fun conservative nodes = (foldl (fn (n,b) => if ( (getDegree n) >= k) then b+1 else b)  0 nodes) < k (*Briggs*)
                                    
    fun getAlias n = if pertSet(coalescedNodes,n) then getAlias(find(!alias,n)) else n

    fun combine (u,v) =( if pertSet(freezeWorklist,v) then deleteSet(freezeWorklist,v) else deleteSet(spillWorklist,v); 
                         addSet(coalescedNodes,v);
                         addDict(alias,v,u);
    (*cambie nodeMoves(func) por moveList(dict)-- ver pag 259!! Que hay en la pag 259?? Pablo*)                     addDict(moveList,u, union(getDict(moveList,u,empty tigergraph.cmp),getDict(moveList,v,empty tigergraph.cmp)));
                       (*   enableMoves(singleton String.compare v);
                       *)   
                            Splayset.app (fn x => print (" "^x))   (adjacent v)
                            ;print("\n");                            
                            app (fn t => (addEdge t u; decrementDegree t)) (adjacent v);
                        if (getDegree u) >= k andalso pertSet(freezeWorklist,u) then (deleteSet(freezeWorklist,u);addSet(spillWorklist,u) ) else () )

     
    fun src m = let val tigerflow.FGRAPH{use,...} = (#1 (!fg_nodos)) in hd (find(use,m) ) end

    fun dst m = let val tigerflow.FGRAPH{def,...} = (#1 (!fg_nodos)) in hd ( find(def,m) ) end

    fun coalesce() = let val m = takeSet(worklistMoves)
                         val (x,y) = (getAlias (src m), getAlias (dst m)) 
                         val (u,v) = if member(precolored, y) then (y,x) else (x,y)
                       in deleteSet(worklistMoves, m);
                           if (u = v) then
                           (*   (print ("\nu1  "^u^" "^v^"\n"); *)
                              (addSet(coalescedMoves,m); 
                              addWorkList u) 
                           else if member(precolored,v) orelse pertSet(adjSet,(u,v)) then 
                                   (addSet(constrainedMoves,m); 
                                   addWorkList u;
                                   addWorkList v) 
                           else if (member(precolored,u) andalso (foldl (fn (t,b) => b andalso ok(t,u)) true  (adjacent v) )) orelse (not(member(precolored,u)) andalso conservative (union ( adjacent u,adjacent v ))) then 
                                    (print ("\nu2  "^u^" "^v^"\n");
                                    addSet(coalescedMoves,m); 
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

    fun spillCost n = Real.fromInt (getDict (countUsesDefs,n,0)) / Real.fromInt (getDegree n) 
    (* SpillCost:
    One Heuristic:
    – Cost = [(# defs & uses)*10 loop-nest-depth ]/degree

    Pablo: Por simplicidad se podría usar algo que minimize los usos/def y/o maximize el grado.
    Marga: seria dejar solo [(# defs & uses)]/degree
    *)

    fun selectSpill () = let val m = foldl (fn (i,ac) => (if spillCost(i) < spillCost(ac) then i else ac)) (takeSet (spillWorklist)) (!spillWorklist)
					     in deleteSet (spillWorklist, m); addSet(simplifyWorklist,m); freezeMoves m end


    fun spillear () = let val (lInstr', tlist) = tigerspill.spill (toList spilledNodes) frame (!lInstr) 
                        in (*raise Fail ("Spilleandooo!");*)lInstr := lInstr' ; addList(empty tigertemp.cmpt, tlist) end

    fun rewriteProgram () = let val newTemps = spillear()
						    in vaciar (spilledNodes,tigertemp.cmpt); initial := union (!coloredNodes, (union (!coalescedNodes, newTemps))); vaciar (coloredNodes,tigertemp.cmpt); vaciar (coalescedNodes,tigertemp.cmpt)  end
            

    fun assignColors() = let fun colorea n = if member(notcolored,n) then () else let val okColors = ref (addList(empty Int.compare, List.tabulate(k, fn n =>n)) ) 
                                            in app (fn w => if member(union(!coloredNodes, precolored), getAlias w) then deleteSet(okColors, getDict(color,getAlias w, k+1)) else ()) (getDict(adjList,n,empty cmpt)) ;
    (print ("\n"^n) (*Splayset.app (fn x => print (" "^Int.toString(x) )) (!okColors); print (" "^(Int.toString(takeSet(okColors)))) *));
      	
                                               if not (hayElem okColors) then addSet(spilledNodes,n) else (addSet(coloredNodes,n); addDict (color,n, takeSet okColors))
                                            end
                            fun repeat () = if not(isEmptyStack selectStack) then (colorea (pop selectStack); repeat ()) else ()
                            in repeat(); app (fn n => addDict(color,n,getDict(color,getAlias n, k-1)) ) (!coalescedNodes) end

    fun livenessAnalysis() =( fg_nodos := tigerflow.makeGraph (!lInstr) ; ig_liveOut := tigerliveness.interferenceGraph (#1 (!fg_nodos)) )

    fun main () = let fun repeat() = if hayElem(simplifyWorklist) then (simplify(); repeat())
                                     else if hayElem(worklistMoves) then (coalesce(); repeat())
                                     else if hayElem(freezeWorklist) then (freeze(); repeat())
                                     else if hayElem(spillWorklist) then (selectSpill();repeat()) else ()
                 in  Splayset.foldl (fn (x,n) => (addDict(color,x,n);n+1)) 0 precolored;
                     Splayset.foldl (fn (x,n) => (addDict(color,x,n);n+1)) k notcolored;
                     print("Maiiiin\n");
                     livenessAnalysis();
                     build();
                     makeWorkList();
                     repeat();      
                     assignColors();
                     let val initial2 = let val tigerliveness.IGRAPH{graph,gtemp,...} = (#1 (!ig_liveOut))
                        in print("test24\n");ref (difference((addList (empty tigertemp.cmpt, List.map (gtemp) (tigergraph.nodes graph))), union(precolored, notcolored))) end
                     in Splayset.app (fn x => print (x^" "^Int.toString (getDict(color,getAlias x, k+10))^"\n")) (!initial2) end;
                     if hayElem(spilledNodes) then (rewriteProgram() ; main() ) else () end  

    in main();
       (!lInstr, Splaymap.map (fn (_,a) => List.nth ((listItems(precolored)@listItems(notcolored)), a) ) (!color) )
 end

end
