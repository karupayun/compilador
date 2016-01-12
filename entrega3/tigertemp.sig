signature tigertemp = sig
    type label = string
    type temp = string
    val makeString: string -> string
    val newtemp: unit -> temp
    val newlabel: unit -> label
    val cmpt : temp * temp -> order (*se necesita esta funcion para poder crear conjuntos de temps*)
end
