signature tigerflow = 
sig
    structure graph
    type instruc
    type table
    type node

    datatype flowgraph = FGRAPH of {control: graph,
				    def: Temp.temp list table,
				    use: Temp.temp list table,
				    ismove: bool table}

    val makeGraph: instruc list -> flowgraph * node list
end
