signature tigercolor = 
sig
	structure frame : tigerframe.frame

	type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict (* Cada temp a su registro *)
    
	val color: {interferencia:  Liveness.igraph,
				initial: allocation,
				spillCost: tigergraph.node -> int,
				registers: frame.register list) -> allocation * tigertemp.temp list

    interferencia = fst (interferenceGraph (makeGraph ..bla bla))
    initial = []
    spillCost x = 1
    registers = [rax,...,]

    val precolored = addList ((empty String.compare), registers)
    val k = numItems precolored
    val initial = ??????
    val simplifyWorklist = empty String.compare
    val freezeWorklist = empty String.compare
    val spillWorklist = ref (empty String.compare)
    val spilledNodes = ref (empty String.compare)
    val coalescedNodes = ref (empty String.compare)
    val coloredNodes = ref (empty String.compare)
    val selectStack = []
    

    val coalescedMoves 

end
