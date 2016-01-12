structure tigerspill :> tigerspill =
struct
open tigerassem

fun spill spillList frame instrs  = let
    (* Las estructuras donde se almacenará el resultado*)
    val ilist = ref ([]:(instr list))
    val tlist = ref ([]:(tigertemp.temp list))
    fun emit x = ilist := x::(!ilist)
    fun newt() = let val t = tigertemp.newtemp() in tlist := t::(!tlist) ; t end

    (* Los accesos de cada temporario a spillear *)
    val spillAlloc = List.map ( fn(x) => (x,tigerframe.allocLocal frame true) ) spillList
    fun peek t = Option.map (fn(x,a)=>tigerframe.offset a) ( List.find ( fn(x,a) => x=t ) spillAlloc )
    
    (* Funciones que generan instrucciones que cargan de un temporario a memoria y viceversa *)
    fun load t k = emit(OPER{assem = "movq "^(Int.toString k)^"(%'s0), %'d0\n", src=[tigerframe.sp],dst=[t],jump=NONE})
    fun store t k = emit(OPER{assem = "movq %'s1, "^(Int.toString k)^"(%'s0)\n", src=[tigerframe.sp,t],dst=[],jump=NONE})

    (* El corazon de nuestro algoritmo: como tratamos una instruccion *)
    fun oneinstr (OPER{src,dst,assem,jump}) = let
        fun auxf x = Option.map (fn k=>(k,newt())) (peek x)
        val auxsrc = List.map auxf src
        val auxdst = List.map auxf dst
        fun newf aux old = List.map (fn(x,y) => Option.getOpt(Option.map #2 x,y)) (ListPair.zip( aux, old ))
        val newsrc = newf auxsrc src
        val newdst = newf auxdst dst
        fun exec f l = List.app ( fn(SOME (k,t)) => f t k | NONE => () ) l
        in exec load auxsrc ; emit(OPER{assem=assem,src=newsrc,dst=newdst,jump=jump}) ; exec store auxdst  end
      | oneinstr (inst as MOVE{src,dst,...}) = (* Este caso lo hacemos distinto para no generar tantos temporarios nuevos *)
            ( case (peek src,peek dst) of
                   (NONE,NONE) => emit(inst)
                 | (SOME k,NONE) => load dst k
                 | (NONE,SOME k) => store src k
                 | (SOME k1, SOME k2) => let val t = newt() in load t k1 ; store t k2 end )
      | oneinstr (LABEL x) = emit(LABEL x)
    in List.app oneinstr instrs ; (!ilist,!tlist) end
end
