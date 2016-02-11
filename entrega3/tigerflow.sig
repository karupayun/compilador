signature tigerflow = 
sig
    (*type instr*)
    type temp

    datatype flowgraph = FGRAPH of {control: tigergraph.graph,
				                    def: (tigergraph.node, tigertemp.temp list) Splaymap.dict,
				                    use: (tigergraph.node, tigertemp.temp list) Splaymap.dict,
				                    ismove: (tigergraph.node, bool) Splaymap.dict}

    val makeGraph: tigerassem.instr list -> flowgraph * tigergraph.node list
end
