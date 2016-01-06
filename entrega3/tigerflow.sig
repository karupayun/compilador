signature tigerflow = 
sig
    type graph
    type temp

    datatype flowgraph = FGRAPH of {control: graph,
				    def: temp list tigergraph.mapnode,
				    use: temp list tigergraph.mapnode,
				    ismove: bool tigergraph.mapnode}

  (*  val makegraph: Assem.instr*)

end
