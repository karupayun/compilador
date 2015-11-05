Clase 8/10

Coloreo de grafos 
La idea es tratar a los temporarios y sus interferencias (dos temporarios interfieren si deben estar vivos en la misma instruccion) como los nodos y aristas respectivamemte de un grado.
Si tenemos k registros disponibles, vemos si se puede colorear el grafo con k colores, de manera que dos nodos adyacentes no tengan el mismo color. Si esto es posible, podremos meter todos los temporarios en registros. Esto no siempre sera posible, y en estos casos mandaremos algun temporario a memoria (spill), achicando las interferencias. 
En el proceso, algunos temporarios se pueden coalescer(pueden ocupar el mismo registro fisico).
Luego de bastante laburo, conseguimos un programa decente.

Fases del coloreo. (Cap 11 - pag 228)
-build (construye el grafo de interferencias)
-simplify (se puede hacer varias veces) 
-coalesce (se puede volver a simplify)
-freeze (se puede volver a simplify)
-potential spill (se puede volver a simplify)
-select (se puede hacer varias veces)
-actual spill (se puede volver al select si es necesario). Despues hay que volver a build

Nota: el algoritmo de coloreo se da en un pseudocodigo imperativo, que hay que traducir a ML.
Sugerencia: usar referencias (datos mutables). En particular, los modulos Splayset y Splaymap (en la ultima version de MOSML, 2.10, aparecen, ademas Rbset y Redblackmap). 
Las construcciones sintacticas del pseudocodigo se pueden emular. Por ej:
    for i in C do codigo  === modulo.app (fn i => codigo) C
    if   then    else     ===  if   then    else     
    
    
Estructuras de datos necesarias

Work-lists, sets y stacks de nodos:
-precolored(ej: rv, args, etc.. no se ponen el fp ni sp)
-initial, los temporarios excepto los coloreados.
-simplifyWorkList, lista de temporarios con bajo grado (el grado de un nodo es el nro de arista que tiene, ie con cuantos nodos interfiere).
-freezeWorkList: nodos de gradobajo, relacionados con moves. (candidatos a coalescer)
-spillWorkList: nodos de grado alto. 
-spilledNodes: nodos para spill.
-coalescedNodes:  registros coalescidos (si u<-v, o u o v van aqui pero no ambos, el otro va WorkList).
-coloredNodes: nodos ya coloreados.
-selectStack: pila con nodos removidos del grafo.   
Todas estas estructuras son disjuntas. 

Sets asociados a moves:
-coalescedMoves: moves coalescidos.
-constrainedMoves: moves con src/dst que interfieren.
-frozenMoves: moves que no van a coalescer.
-workListMoves: moves que pueden coalescer.
-activeMoves: moves que no estan listos para coalescer. (en un principio todos los moves estan aca)
Todas estas estructuras son disjuntas. 

Otros:
-adjSet: conjunto de aristas (interferencias) de la forma (u,v). Es simetrico: Si (u,v) esta en adjSet entonces (v,u) esta en adjSet.
-adjList: conjunto de nodos adyacentes. adjList[u] da los adyacentes a u.
-degree: grado de un nodo. degree[u] da el grado de u.
-moveList: moves asociados a un nodo. moveList[u] da los moves que usan a u.
-alias: cuando un move(u,v)y u va a coalexedNodes, alias(u) = v.
-color: color elegido para un nodo.


Algoritmo:

procedure main()
    LivenessAnalysis()
    Build()
    MakeWorkList()
    repeat
        if simplifyWorkList <> {} then Simplify()
        else if workListMoves <> {} then Coalesce()
        else if freezeWorkList <> {} then Freeze() 
        else if spillWorkList <> {} then SelectSpill()
    until simplifyWorkList = {} and workListMove = {} 
          and freezeWorkList = {} and spillWorkList = {} 
    AssignColors()
    if spilledNodes <> {} then 
        RewriteProgram(spillesNodes)
        Main()
        
Functores
Una funcion es algo que toma un valor y entrega otro valor. Un functor toma una o mas estructuras (modulos) y entrega otra estructura. Recordar que las estructuras tienen signaturas.
Ej:
    fun  qs [] = []
    | qs (h::t) = let val (m, M) = List.partition (fn x => x<h) t
                 in qs m@[h]@qs M end
Ya visto (primer clase).
Para compilar con un functor usar la opcion -toplevel.

LivenessAnalysis ya fue visto.

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
                simplifyWorkList<- simplifyWorkLis U {n}

procedure Simplify()
    let n en simplifyWorkList  (*tomo uno cualquiera de simplifyWorkList*)                             
        simplifyWorkList <- simplifyWorkList \ {n}
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
                simplifyWorkList <- simplifyWorkList U {m}
                
procedure EnableMoves(nodes)
    forall n en nodes
        forall m en NodeMoves(n)
            if m en actualMoves then
                activeMoves <- activeMoves \ {m}
                workListMoves <- workListMoves U {m}                        
                                    
        
                                                
