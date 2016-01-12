structure tigercodegen :> tigercodegen =
struct

open tigerassem
val aMOVE = MOVE
val aLABEL = LABEL
open tigertree
open tigerframe
open tigertemp

fun codegen stm = (*se aplica a cada funcion*)
    let val ilist = ref ([]:(instr list)) (*lista de instrucciones que va a ir mutando*)
        fun emit x = ilist := x::(!ilist) (*!ilist es equivalente a *ilist en C y ilist := a es equivalente a *ilist = a en C*)
        fun result gen = let val t = tigertemp.newtemp() in (gen t; t) end
        (* val saveregs = List.map (fn x => result (fn nt => (emit(aMOVE{assem = "movq %'s0, %'d0\n", src=x, dst=nt}) ; nt) ))
        fun recoverregs dst src = List.map (fn (d,s) => emit(aMOVE{assem = "movq %'s0, %'d0\n", src=s, dst=d}) ) ( ListPair.zip(dst,src) ) *)
        fun munchStm (SEQ (a,b)) = (munchStm a; munchStm b)
        |   munchStm (MOVE (MEM e1, e2)) = emit(OPER{assem = "movq %'s0, (%'s1)\n", src=[munchExp e2,munchExp e1],dst=[],jump=NONE})
        |   munchStm (MOVE (TEMP i, e2)) = emit(aMOVE{assem = "movq %'s0, %'d0\n", src=munchExp e2, dst=i})
        |   munchStm (LABEL lab)        = emit(aLABEL{assem = (makeString lab) ^ ":\n", lab = lab }) (* DUDA: esto está bien? el libro hace algo bastaaaaante raro. PAB *)
        |   munchStm (JUMP (NAME l, [lp])) = if l <> lp then raise Fail "Esto no deberia suceder m33\n" else 
            emit(OPER{assem="jmp 'j0\n", src=[], dst=[], jump=SOME [l]})
        |   munchStm (JUMP _) = raise Fail "Esto no deberia suceder m22\n"
        |   munchStm (CJUMP (rop, e1, e2, l1, l2)) =
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
                in emit(OPER{assem = "cmpq %'s1, %'s0\n", src=[munchExp e1, munchExp e2], dst= [], jump=NONE}); emit(OPER{assem = (salto rop) ^ " 'j0^\n", src = [], dst = [], jump = SOME [l1,l2]}) end
        |   munchStm (EXP (CALL (NAME lab,args))) = ( emit(OPER{assem="call "^(makeString lab)^"\n", src=munchArgs(0,args), dst=tigerframe.calldefs, jump=NONE}) ;
                                                    let val spoffset = (List.length args - List.length tigerframe.argregs)*tigerframe.wSz (* vamos a recuperar el sp en caso de haber hecho pushq antes del call*)
                                                    in if spoffset>0 then emit(OPER{assem = "addq $"^(Int.toString spoffset)^", %'d0\n", src = [tigerframe.sp], dst = [tigerframe.sp], jump = NONE}) else () end )
        |   munchStm (EXP _) = raise Fail "Creemos que esto no deberia suceder ?\n" (*DUDA: puede suceder esto? mariano *)
        |   munchStm _ = raise Fail "Casos no cubiertos en tigercodegen.munchStm" 
        and munchExp (CONST i) = result (fn r => emit(OPER{assem = "movq $"^(Int.toString i)^", %'d0\n", src = [], dst = [r], jump = NONE}))
        |   munchExp (NAME lab) = result (fn r => emit(OPER{assem = "movq $"^(makeString lab)^", %'d0\n", src = [], dst = [r], jump = NONE})) (* Con Mariano suponemos que esto no puede aparecer pero por si las dudas ... *)
        |   munchExp (MEM m) = result (fn r => emit(OPER{assem = "movq (%'s0), %'s1\n", src =[munchExp m,r] , dst =[], jump=NONE}))
        |   munchExp (TEMP t) = t 
        |   munchExp (CALL _) = raise Fail "Este caso CALL no debería aparecer por el canonizar"
        |   munchExp (ESEQ _) = raise Fail "Este caso ESEQ no debería aparecer por el canonizar"
        |   munchExp (BINOP (PLUS, CONST i, e1)) = result ( fn r => (emit(aMOVE{assem = "movq %'s0, %'d0\n", src = munchExp e1, dst=r}); emit(OPER{assem = "addq $"^(Int.toString i)^", %'d0\n", src = [r], dst = [r], jump = NONE}))) (*el libro dice de hacerlo asi y esperar q dsp a r y munchExp e1 se le asigne el mismo registro, peor no entiendo por q. RtaZ: Ese move tiene que estar si o si. Después lo de "esperar" tiene que ser algo que hagamos nosotros con el register allocator. De esa forma esa instrucción MOVE puede desaparecer, se entiende? Si nosotros somos cracks la deberíamos hacer desaparecer. *) 
        |   munchExp (BINOP (PLUS, e1, CONST i)) = result ( fn r => (emit(aMOVE{assem = "movq %'s0, %'d0\n", src = munchExp e1, dst=r}); emit(OPER{assem = "addq $"^(Int.toString i)^", %'d0\n", src = [r], dst = [r], jump = NONE})))
        |   munchExp (BINOP (PLUS, e1, e2)) = result ( fn r => (emit(aMOVE{assem = "movq %'s0, %'d0\n", src=munchExp e1, dst=r}); emit(OPER{assem = "addq %'s1, %'d0\n", src = [r, munchExp e2], dst = [r], jump = NONE})))
        |   munchExp (BINOP (MINUS, CONST i, e1)) = result ( fn r => (emit(aMOVE{assem = "movq %'s0, %'d0\n", src = munchExp e1, dst=r}); emit(OPER{assem = "subq $"^(Int.toString i)^", %'d0\n", src = [r], dst = [r], jump = NONE})))
        |   munchExp (BINOP (MINUS, e1, CONST i)) = result ( fn r => (emit(aMOVE{assem = "movq %'s0, %'d0\n", src = munchExp e1, dst=r}); emit(OPER{assem = "subq $"^(Int.toString i)^", %'d0\n", src = [r], dst = [r], jump = NONE})))
        |   munchExp (BINOP (MINUS, e1, e2)) = result ( fn r => (emit(aMOVE{assem = "movq %'s0, %'d0\n", src=munchExp e1, dst=r}); emit(OPER{assem = "subq %'s1, %'d0\n", src = [r, munchExp e2], dst = [r], jump = NONE})))
        |   munchExp (BINOP (MUL, CONST i, e1)) = result ( fn r => (emit(aMOVE{assem = "movq %'s0, %'d0\n", src=munchExp e1, dst=r}); emit(OPER{assem = "imulq $"^(Int.toString i)^", %'d0\n", src = [r], dst = [r, tigerframe.rdx], jump = NONE})))
        |   munchExp (BINOP (MUL, e1, CONST i)) = result ( fn r => (emit(aMOVE{assem = "movq %'s0, %'d0\n", src=munchExp e1, dst=r}); emit(OPER{assem = "imulq $"^(Int.toString i)^", %'d0\n", src = [r], dst = [r, tigerframe.rdx], jump = NONE})))
        |   munchExp (BINOP (MUL, e1, e2)) = result ( fn r => (emit(aMOVE{assem = "movq %'s0, %'d0\n", src=munchExp e1, dst=r}); emit(OPER{assem = "imulq %'s1, %'d0\n", src = [r, munchExp e2], dst = [r, tigerframe.rdx], jump = NONE}))) 
        |   munchExp (BINOP (DIV, CONST i, e1)) = result ( fn r => (emit(OPER{assem = "movq $"^(Int.toString i)^", %'d0\n", src=[], dst=[tigerframe.rax], jump = NONE}); emit(OPER{assem = "idivq %s1'\n", src = [tigerframe.rax, munchExp e1], dst = [tigerframe.rax, tigerframe.rdx], jump = NONE}); emit(aMOVE{assem = "movq %'s0, %'d0\n", src=tigerframe.rax, dst=r} )))
        |   munchExp (BINOP (DIV, e1, CONST i)) = result ( fn r => (  emit(aMOVE{assem = "movq %'s0, %'d0\n", src=munchExp e1, dst=tigerframe.rax} ); emit(OPER{assem = "movq $"^(Int.toString i)^", %'d0\n", src=[], dst=[r], jump = NONE}); emit(OPER{assem = "idivq %'s1\n", src = [tigerframe.rax, r], dst = [tigerframe.rax, tigerframe.rdx], jump = NONE}); emit(aMOVE{assem = "movq %'s0, %'d0\n", src=tigerframe.rax, dst=r} )  ) ) 
        |   munchExp (BINOP (DIV, e1, e2)) = result ( fn r => (emit(aMOVE{assem = "movq %'s0, %'d0\n", src=munchExp e1, dst=tigerframe.rax} ); emit(OPER{assem = "idivq %'s1\n", src = [tigerframe.rax, munchExp e2], dst = [tigerframe.rax, tigerframe.rdx], jump = NONE}); emit(aMOVE{assem = "movq %'s0, %'d0\n", src=tigerframe.rax, dst=r}) ))
        |   munchExp _ = raise Fail "TODO"
        and munchArgs(_,[]) = []
        |   munchArgs(n,x::xs) =
                if n < List.length tigerframe.argregs then
                      let val r = List.nth(tigerframe.argregs,n) in ( emit(aMOVE{assem = "movq %'s0, %'d0\n", src=munchExp x, dst=r}) ; r :: munchArgs(n+1,xs) ) end
                else ( emit(OPER{assem = "pushq %'s0\n", src=[ munchExp x] , dst=[], jump=NONE}) ; munchArgs(n+1,xs) ) 
        in munchStm stm ; rev(!ilist) end

fun codegens stms = List.foldr (fn(a,b)=>a@b) [] (List.map codegen stms)

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
end 
