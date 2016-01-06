signature tigercodegen = sig
val codegen : tigertree.stm -> tigerassem.instr list
val codegens : tigertree.stm list -> tigerassem.instr list
end
