structure tigerspill :> tigerspill =
struct
open tigerassem

fun spill spillList frame instrs  = let
    val ilist = ref ([]:(instr list))
    val tlist = ref ([]:(tigertemp.temp list))
    fun emit x = ilist := x::(!ilist)
    fun newt() = let val t = tigertemp.newtemp() in tlist := t::(!tlist) ; t end

    fun oneinstr (OPER{src,dst,...}) =raise Fail ""
      | oneinstr (MOVE{src,dst,...}) = raise Fail ""
      | oneinstr (LABEL x) = emit(LABEL x)
    in List.app oneinstr instrs ; (!ilist,!tlist) end
end
