fun max(x,y) = if x>y then x else y
fun maxargs = maxargs' "print"
fun maxargs' s (CallExp (r,_)) = foldl max (if r.func==s then length(r.args) else 0) (map (maxargs' s) r.args)

