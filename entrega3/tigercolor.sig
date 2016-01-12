signature tigercolor = 
sig
	structure frame : tigerframe.frame

	type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict (* Cada temp a su registro *)
    
	val color: {interferencia:  Liveness.igraph,
								initial: allocation,
								spillCost: tigergraph.node -> int,
								registers: frame.register list) -> allocation * tigertemp.temp list
end
