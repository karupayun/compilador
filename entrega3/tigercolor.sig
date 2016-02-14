signature tigercolor = 
sig


	type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict (* Cada temp a su registro *)
    
	val alloc : tigerassem.instr list * tigerframe.frame -> tigerassem.instr list * allocation
end

