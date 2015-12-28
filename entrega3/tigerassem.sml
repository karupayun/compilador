structure tigerassem =

struct
type reg = string
type temp = tigertemp.temp
type label = tigertemp.label
    
datatype instr = OPER of {assem=string, src=temp list, dst = temp list, jump label list option }
               | LABEL of {assem = string, lab = label}
               | MOVE of {assem = string, src = temp, dst = temp}
