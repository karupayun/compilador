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

    fun color {interferencia = g, initial = i, spillCost = s, registers = r } = let

 (*ref*)val initial = 

        val map (g.gtemp) (g.graph.nodes) (*Ver como hacer esto *)
        val simplifyWorklist = ref []
        val freezeWorklist = ref (empty String.compare)
        val spillWorklist = ref (empty String.compare)
        val spilledNodes = ref (empty String.compare)
        val coalescedNodes = ref (empty String.compare)
        val coloredNodes = ref (empty String.compare)
        val selectStack = []


        val coalescedMoves = 
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
                else if freezeWorkList <> {} then Freeze() 
                else if spillWorkList <> {} then SelectSpill()
            until not(null(!simplifyWorklist)} andalso workListMove = {} 
                  andalso freezeWorkList = {} andalso spillWorkList = {} 
            AssignColors()
            if spilledNodes <> {} then 
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

        function Adjacent(n)
            adjList[n] \ (selectStack U coalescedNodes)        

        function NodeMoves(u)
            moveList[n] interseccion (activeMoves U workListMoves)
            
        function moveRelated(n)
            NodeMoves <> {}
            
        procedure MakeWorkList()
            forall n en initial
                initial <- initial \{n}
                if degree[n] >= k then (*k es la cantidad de colores*)
                    spillWorkList <- spillWorkList U {n}
                else if moveRelated[n] then 
                        freezeWorkList <- freezeWorkList U {n}
                    else 
                        simplifyWorkList = simplifyWorkList @ [n]

        procedure Simplify()
            let n = hd (simplifyWorkList)                               
                simplifyWorkList = tl( simplifyWorkList )
                push(n,selectStack)
                forall m in Adjacent(n)
                    DecrementDegree(m)
                    
        procedure DecrementDegree(m)
            let d = degree[m]
                degree[m] <- degree[m]-1
                if d = k then (*ahora tiene grado k-1, permite colorear el grafo*)
                    EnableMoves({m}UAdjacent(m))
                    spillWorkList <- spillWorkList \ {m}
                    if MoveRelated(m) then
                        freezeWorkList <- freezeWorkList U {m}
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
                coalescedMoves <- coalescedMoves U {m}
                addWorkList(u)
            else if (v ∈ precolored or (u,v) ∈ adjSet) then
                constrainedMoves <- constrainedMoves U {m}
                addWorkList (u)
                addWorkList (v)        
            else if ((u ∈ precolored and (pt t ∈ adjacent(v), OK (t,u))) or (n ∉ precolored and
                Conservative (Adjacent (u) U Adjacent(v))) then
                    coalescedMoves <- coalescedMoves U {m}
                    Combine (u,v)
                    addWorkList (u)
            else
                activeMoves <- activeMoves U {m}

        procedure AddWorkList (u)
            if (u ∉ precolored and (not MoveRelated (u)) and (degree[u] < K)) then
                freezeWorkList <- freezeWorkList \ {u}
                simplifyWorkList = simplifyWorkList @ [u]
                
        function OK (t,r)
            degree[t] < K or t ∈ precolored or (t,r) ∈ adjSet
            
        function Conservative(nodes)
            let k = 0
            forall n ∈ nodes
                if degree[u] >= K then k <- k+1      
            return (k < K)

        function getAlias(n)
            if n ∈ coalescedNodes then
                GetAlias (alias[n])
            else n

        procedure Combine(u,v)
            if v ∈ freezeWorklist then
                freezeWorklist = freezeWorkList \ {u}
            else
                spillWorklist <- spillWorklist \ {v}
            coalescedNodes <- coalescedNodes U {v}
            alias[v] = u
            nodeMoves[u] = nodeMoves[u] U nodeMoves[v]
            forall t ∈ Adjacent(v)
                AddEdge (t,u)
                DecrementDegree(t)
            if (degree[u] >= k andalso u ∈ freezeWorklist)
                freezeWorklist = freezeWorklist \ {u}
                spillWorklist <- spillWorklist U {u}

        procedure Freeze()
            let u ∈ freezeWorklist
            freezeWorklist \ {u}
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
                    freezeWorklist = freezeWorklist \ {v}
                    simplifyWorklist = simplifyWorklist @ [v]


        procedure SelectSpill ()
            let m ∈ spillWorklist (* Ver nota!! *)
            spillWorklist = spillWorklist \ {m}
            simplifyWorklist = simplifyWorklist U {m}
            FreezeMoves(m)

        procedure AssignColors()
            while SelectStack not empty
                let n = pop(SelectStack)
                okColors = {0, .. , k-1}
                forall w ∈ adjList[n]
                    if GetAlias(w) ∈ (coloredNodes U precolores) then
                        okColors = okColors \ {color[GetAlias(w)]}
                if okColors = {}
                    spilledNodes = spilledNodes U {n}
                else
                    coloredNodes = coloredNodes U {n}
                    let c ∈ okColors
                    color[n] = c
            forall n ∈ coalescedNodes
                color[n] = color[GetAlias(n)]

        procedure RewriteProgram()
            (* HACER Algoritmo de Marian *)
            spilledNodes = {}
            initial = coloredNodes U coalescedNodes U newTemps
            coloredNodes = {}
            coalescedNodes = {}

        in Main()

end
