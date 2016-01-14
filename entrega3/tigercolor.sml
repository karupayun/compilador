signature tigercolor = 
sig
	structure frame : tigerframe.frame

	type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict (* Cada temp a su registro *)
    
	val color: {interferencia:  Liveness.igraph,
				initial: allocation,
				spillCost: tigergraph.node -> int,
				registers: frame.register list) -> allocation * tigertemp.temp list
(*
    interferencia = fst (interferenceGraph (makeGraph ..bla bla))
    initial = []
    spillCost x = 1
    registers = [rax,...,] tigerframe.registers*)

       val precolored = addList ((empty String.compare), r)
       val k = numItems precolored

	fun addSet (s,e) = s := add (!s,e)
	fun hayElem s = not(isEmpty(!s))
	fun deleteSet (s,e) = if member(!s,e) then s:= delete (!s,e) else ()
	fun pertSet (s,e) = member(!s,e)
	fun takeSet (s,e) = e := hd(listItems(!s))
	fun vaciar (s) = s := empty  
	fun toSet s = addList  (empty,!s) 

	fun push (p,e) = p := (!p @ [e])
	fun pop (p,e) = (e := hd(!p) ; p := tail(!p))

	fun colorearCoalesced n = color[n] := color[GetAlias(n)]
	

  fun color {interferencia = g, initial = i, spillCost = s, registers = r } = let

 (*ref*)val initial = 

        val map (g.gtemp) (g.graph.nodes) (*Ver como hacer esto *)

        val simplifyWorklist = ref []
        val freezeWorklist = ref (empty String.compare)
        val spillWorklist = ref (empty String.compare)


        val spilledNodes = ref (empty String.compare)
        val coalescedNodes = ref (empty String.compare)
        val coloredNodes = ref (empty String.compare)
        val selectStack = ref []


        val coalescedMoves = ref (empty String.compare)
        val contrainedMoves = 
        val frozenMoves = 
        val worklistMoves = 
        val activeMoves = 

    
        val adjSet = 
        val adjList = 
        val degree = 
        val moveList = 
        val alias = 
        val color = 
        

        procedure Main()
            LivenessAnalysis()
            Build()
            MakeWorkList()
            repeat
                if not(null(!simplifyWorklist)) then Simplify()
                else if workListMoves <> {} then Coalesce()
                else if hayElem(freezeWorkList) then Freeze() 
                else if hayElem(spillWorkList) then SelectSpill()
            until not(null(!simplifyWorklist)} andalso workListMove = {} 
                  andalso hayElem(!freezeWorkList) andalso hayElem(spillWorkList)
            AssignColors()
            if hayElem(spilledNodes) then 
                RewriteProgram(spillesNodes)
                Main()

        procedure Build():
            forall b en ( blocks in program(function) )
                let live = liveOut(b)
                forall I en ( instrucciones(b) in reverse )
                    if isMoveInstruction(I) then  
                        live = live \ use(I)
                        forall n en (def(I) U use(I))
                            moveList[n] <- moveList[n] U {I} 
                         moveListMoves <- workListMoves U {I}
                    live <- live U def(I)
                    forall d en def(I)
                        forall l en live
                            AddEdge(l,d)
                    live <- use(I) U (live\def(I))   
            
        
        procedure AddEdge(u,v):
            if ((u,v) no esta en adjSet) and (u <> v) then
                adjSet <- adjSet U {(u,v), (v,u)}
                if u no esta en precolored then
                    adjList[u] <- adjList[u] U {v}
                    degree[u] <- degree[u] + 1 
                if v no esta en precolored then
                    adjList[v] <- adjList[v] U {u}
                    degree[v] <- degree[v] + 1   

        function Adjacent(n) = difference ( adjList[n] , addList(!selectStack , !coalescedNodes))        

        function NodeMoves(u)
            moveList[n] interseccion (activeMoves U workListMoves)
            
        function moveRelated(n)
            NodeMoves <> {}
            
        procedure MakeWorkList()
            forall n en initial
                initial <- initial \{n}
                if degree[n] >= k then (*k es la cantidad de colores*)
                    addSet (spillWorkList, n)
                else if moveRelated[n] then 
                        addSet (freezeWorkList, n)
                    else 
                        simplifyWorkList = simplifyWorkList @ [n]

        procedure Simplify()
            let n = hd (simplifyWorkList)                               
                simplifyWorkList = tl( simplifyWorkList )
                push(selectStack,n)
                forall m in Adjacent(n)
                    DecrementDegree(m)
                    
        procedure DecrementDegree(m)
            let d = degree[m]
                degree[m] <- degree[m]-1
                if d = k then (*ahora tiene grado k-1, permite colorear el grafo*)
                    EnableMoves({m}UAdjacent(m))
                    deleteSet(spillWorkList,m)
                    if MoveRelated(m) then
                        addSet (freezeWorkList, m)
                    else 
                        simplifyWorkList = simplifyWorkList @ [m]
                        
        procedure EnableMoves(nodes)
            forall n en nodes
                forall m en NodeMoves(n)
                    if m en actualMoves then
                        activeMoves <- activeMoves \ {m}
                        workListMoves <- workListMoves U {m} 

         procedure Coalesce ()
            let m(=copy(x,y)) ∈ workListMoves
            x <- GetAlias(x)
            y <- GetAlias(y)
            if y ∈ precolored then
                let (u,v) = (y,x)
            else
                let (u,v) = (x,y)
            workListMoves <-workListMoves \ {m}
            if (u = v) then
                addSet (coalescedMoves ,m)
                addWorkList(u)
            else if (v ∈ precolored or (u,v) ∈ adjSet) then
                constrainedMoves <- constrainedMoves U {m}
                addWorkList (u)
                addWorkList (v)        
            else if ((u ∈ precolored and (pt t ∈ adjacent(v), OK (t,u))) or (n ∉ precolored and
                Conservative (Adjacent (u) U Adjacent(v))) then
                    addSet (coalescedMoves ,m)
                    Combine (u,v)
                    addWorkList (u)
            else
                activeMoves <- activeMoves U {m}

        procedure AddWorkList (u)
            if (u ∉ precolored and (not MoveRelated (u)) and (degree[u] < K)) then
                deleteSet (freezeWorkList, u)
                simplifyWorkList = simplifyWorkList @ [u]
                
        function OK (t,r)
            degree[t] < K or t ∈ precolored or (t,r) ∈ adjSet
            
        function Conservative(nodes)
            let k = 0
            forall n ∈ nodes
                if degree[u] >= K then k <- k+1      
            return (k < K)

        function getAlias(n)
            if pertSet(coalescedNodes,n) then
                GetAlias (alias[n])
            else n

        procedure Combine(u,v)
            if pertSet(freezeWorklist,v) then
                deleteSet (freezeWorkList, u)
            else
                deleteSet (spillWorklist,v)
            addSet (coalescedNodes,v)
            alias[v] = u
            nodeMoves[u] = nodeMoves[u] U nodeMoves[v]
            forall t ∈ Adjacent(v)
                AddEdge (t,u)
                DecrementDegree(t)
            if (degree[u] >= k andalso pertSet(freezeWorklist,u))
                deleteSet (freezeWorkList, u)
                addSet (spillWorklist,u)

        procedure Freeze()
            takeSet (freezeWorkList, u)
            deleteSet (freezeWorkList, u)
            simplifyWorklist = simplifyWorklist @ [u]
            FreezeMoves (u)

        procedure FreezeMoves (u)
            forall (m??=(copy(x,y)) ∈ NoveMoves(u))
                if GetAlias(y) = GetAlias(u) then
                    v = GetAlias(x)
                else
                    V = GetAlias (y)
                activeMoves = activeMoves \ {m}
                frozenMoves = frozenMoves U {m}
                if NodeMoves(v) = {} andalso degree[v] < k then
                    deleteSet (freezeWorkList, v)
                    simplifyWorklist = simplifyWorklist @ [v]


        procedure SelectSpill ()
            takeSet (spillWorklist, m) (* Ver nota!! *)
            deleteSet (spillWorklist, m)
            simplifyWorklist = simplifyWorklist @ [m]
            FreezeMoves(m)

        procedure AssignColors()
            while hayElem(SelectStack)
                pop(SelectStack,n)
                okColors = {0, .. , k-1}
                forall w ∈ adjList[n]
                    if pertSet( union (coloredNodes, precolored), GetAlias(w)) then
                        okColors = okColors \ {color[GetAlias(w)]}
                if okColors = {}
                    addSet (spilledNodes,n)
                else
                    addSe (coloredNodes,n)
                    let c ∈ okColors
                    color[n] = c
			map colorearCoalesced (listItems (!coalescedNodes))


        procedure RewriteProgram()
            (* HACER Algoritmo de Marian *)
            vaciar (spilledNodes)
            initial = union (!coloredNodes, (union (!coalescedNodes, !newTemps)))
            vaciar (coloredNodes)
            vaciar (coalescedNodes) 

        in Main()

end
