signature tigerflow = 
sig
    type instr
    type 'a table
    type node
    type temp
    type graph


    datatype flowgraph = FGRAPH of {control: graph,
				    def: temp list table,
				    use: temp list table,
				    ismove: bool table}

(*    val makeGraph: instr list -> flowgraph * node list    HACIENDOLA*)
end
