structure ej4 =
struct
open tigerabs
val maxargs = let fun max(x,y) = if x>y then x else y
                  fun maxL l = foldl max 0 l
                  fun maxargsL s exps = maxL (map (maxargs' s) exps)
                  and maxargs' s (VarExp _) = 0
                  |   maxargs' s (UnitExp _) = 0
                  |   maxargs' s (NilExp _) = 0
                  |   maxargs' s (IntExp _) = 0
                  |   maxargs' s (StringExp _) = 0
                  |   maxargs' s (CallExp ({args, func},_)) = max((if (func = s) then List.length args else 0), maxargsL s args)
                  |   maxargs' s (OpExp ({left, right, ...},_)) = max (maxargs' s left, maxargs' s right)
                  |   maxargs' s (RecordExp ({fields, ...},_)) = maxargsL s (map #2 fields) 
                  |   maxargs' s (SeqExp (l,_)) = maxargsL s l
                  |   maxargs' s (AssignExp ({exp, ...},_)) = maxargs' s exp
                  |   maxargs' s (IfExp ({test, then', else'},_)) = maxargsL s [then', test, (Option.getOpt (else', UnitExp 8))]
                  |   maxargs' s (LetExp ({body, decs},_)) = max ( maxargs' s body, foldl max 0 (map (maxdec s) decs) ) 
                  |   maxargs' s (BreakExp _) = 0
                  |   maxargs' s (ArrayExp ({size, init, ...},_)) = max (maxargs' s size, maxargs' s init)
                  |   maxargs' s (WhileExp ({test, body },_)) = max (maxargs' s test, maxargs' s body)
                  |   maxargs' s (ForExp ({lo,hi,body,...},_)) = maxargsL s [lo,hi,body]
                  and maxdec s (FunctionDec l) = maxargsL s (map (#body o #1) l)
                  |   maxdec s (VarDec ({init,...},_)) = maxargs' s init
                  |   maxdec s (TypeDec _) = 0
                  in
                    maxargs' "print"
              end
              
end
