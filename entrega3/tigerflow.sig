signature tigerflow = 
sig
    type graph

    datatype flowgraph = FGRAPH of {control: graph,
				    def: Temp.temp list Graph.Table.table,
				    use: Temp.temp list Graph.Table.table,
				    ismove: bool Graph.Table.table}

end
