structure tigercodegen :> tigercodegen =
struct
	
open tigertree
open tigerassem
open tigerframe

fun codegen frame stm = (*se aplica a cada funcion*)
    let val ilist = ref ([]:(instr list)) (*lista de instrucciones que va a ir mutando*)
        fun emit x = ilist := x::(!ilist) (*!ilist es equivalente a *ilist en C y ilist := a es equivalente a *ilist = a en C*)
        fun results gen = let val t = tigertemp.newtemp
                            in (gen t; t) end
        fun munchStm (SEQ (a,b)) = (munchStm a; munchStm b)
        |   munchStm (MOVE (MEM e1, e2)) = emit(OPER{assem = "movq %'s0, (%'s1)\n", src=[munchExp e2,munchExp e1],dst=[],jump=NONE})
        |   munchStm (MOVE (TEMP i, e2)) = emit(MOVE{assem = "movq %'s0, %'d0\n", src=munchExp e2, dst=i})
        |   munchStm (LABEL lab)        = emit(LABEL{assem = lab ^ ":\n", lab = lab})
        |   munchStm (JUMP (NAME l, [lp])) = if l <> lp then raise Fail "Esto no deberia suceder m33\n" else 
            emit(OPER{assem="jmp "^(tigertemp.makeString l)^"\n", src=[], dst=[], jump=SOME [l]})
        |   munchStm (JUMP _) = raise Fail "Esto no deberia suceder m22\n"
        |   munchStm (CJUMP (op, e1, e2, l1, l2)) =
                let fun salto EQ = "je" 
	                  | salto NE = "jne"
                      | salto LT = "jl"
                      | salto GE = "jge"
                      | salto GT = "jg"
                      | salto LE = "jle"
                      | salto ULT = "jb"
                      | salto UGE = "ja"
                      | salto ULE = "jbe"
                      | salto UGT = "jae"
                in emit(OPER{assem = "cmpq %'s1, %'s0\n", src=[munchExp e1, munchExp e2], dst= [], jump=NONE); emit(OPER{assem = (salto op) ^ " " ^(tigertemp.makeString l1)^"\n", src = [], dst = [], jump = SOME [l1,l2]}) end
        |   munchStm (CJUMP _) = raise Fail "Esto no deberia suceder m23\n"
        |   munchStm (EXP _) = raise Fail "Creemos que esto no deberia suceder (?\n" (*DUDA: puede suceder esto? mariano *)
        and munchExp (CONST i) = result (fn r => emit(OPER{assem = "movq $"^(Int.toString i)^", %'d0\n", src = [], dst = [r], jump = NONE}))
        |   munchExp () =
        |   munchExp () =
        |   munchExp () =
        |   munchExp () =
          



(* DUDA: no entendemos un carajo lo del SP y FP mariano y pablo
            |(MOVE (TEMP t1, BINOP(MINUS, TEMP t2, CONST i)) ) => 
                if t1 = tigerframe.sp andalso t2 = tigerframe.sp then (*fp y sp no tienen que aparecer en ningun momento en src ni dst porque no pueden ser elegidos para guardar un estado intermedio*)
                    emit(OPER{assem = "MOV SP, SP-"^Int.toString i^"\n", src = [], dst = [], jump = NONE}
                else 
                    emit(OPER{assem = "MOV 'd0, 's0-"^Int.toString i^"\n", src = [t2], dst = [t1], jump = NONE}
            | MOVE (MEM e1, MEM e2) => (* si no tenemos mem -> mem generamos t<-mem1 seguido de mem2<-t*)
                let val t = tigertemp.newtemp()
                in emit(OPER{assem = "MOV 'd0, MEM['s0]\n", dst=[t], src = [munchExp e2], jump = NONE} ); emit(OPER{assem = "MOV MEM['d0], 's0\n", dst=[munchExp e1], src = [t], jump = NONE} ) 
                end
*)

(*sacado de la clase de Guido *)
(*Si generamos código intermedio de la forma:
    MOVE (MEM (CONST i), CONST j).
Se captura con 
    | MOVE (MEM .... ) =>
        emit ( OPER {assem = "MOV M["^Int.toString i^",$"^Int.toString j^"\n", src = [], dst = [], jump = NONE})
    | MOVE (TEMP t1, TEMP t2)
        emit ( MOVE {assem = "MOV 'd0, 's0\n", src = t2, dst=t1})
    | MOE (TEMP t, e) = 
        emit (MOVE {assem = "MOV 'd0, 's0\n", src = {munchExp e, dst = t}) *) 
