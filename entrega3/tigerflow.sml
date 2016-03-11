structure tigerflow :> tigerflow =
struct
    open tigergraph
    open tigerassem
    open tigertab
    type temp = tigertemp.temp

    datatype flowgraph = FGRAPH of {control: graph,
                                    def: (node, temp list) Splaymap.dict (*(node * temp list) list*),
                                    use: (node, temp list) Splaymap.dict (*(node * temp list) list*),
                                    ismove: (node, bool) Splaymap.dict (*(node * bool) list*)}

(*no se como crear tablas donde la clave sean de tipo node, creo que el problema esta en que hay un array pero no estoy segura. asi que cambia 	    def: tigertemp.temp list tigergraph.mapnode,
			    use: tigertemp.temp list tigergraph.mapnode,
			    ismove: bool tigergraph.mapnode
a
                def: tigergraph.node * tigertemp.temp list,
			    use: tigergraph.node * tigertemp.temp list,
			    ismove: tigergraph.node * bool
*)
  (* Note:  any "use" within the block is assumed to be BEFORE a "def" 
        of the same variable.  If there is a def(x) followed by use(x)
       in the same block, do not mention the use in this data structure,
       mention only the def.

     More generally:
       If there are any nonzero number of defs, mention def(x).
       If there are any nonzero number of uses BEFORE THE FIRST DEF,
           mention use(x).

     For any node in the graph,  
           Graph.Table.look(def,node) = SOME(def-list)
           Graph.Table.look(use,node) = SOME(use-list)
   *)

    (*MakeGraph va a venir acá para no hacer un modulo unitario *)


fun makeGraph lInstr = 
   let val gc = newGraph()
            (*mktln crea los nodos del grafo de control y los asocia con la instr correspondiente (tn) y crea una tabla que mapea cada label con el nodo al que corresponde (tl) *)
       val printinstr = tigerassem.format (fn x => x)
       fun printbody instrs = List.foldr (fn(a,b)=>a^"\n"^b) "" (List.map printinstr instrs)
        val _ = print(printbody lInstr )
        fun mktln [] tl tn = (tl,tn) 
         | mktln (LABEL{assem, lab}::inss) tl tn = let val (tl', tn') = mktln inss (tabNueva()) []
                                                            val v = case tn' of
                                                                      [] => raise Fail ("error lab!")
                                                                    | ((n,_)::ns) => n          
                                                            val tn_new = tn@tn'
                                                            val tl'' = tabInserta (lab, v,tl)   
                                                            val tl_new = tabInserList(tl'',(tabAList tl'))
                                                        in (tl_new, tn_new) end       
         | mktln (i::inss) tl tn = mktln inss tl (tn @ [(newNode(gc), i)] )

		   val (tablab,tabnode) = mktln lInstr (tabNueva(): (string, node) Tabla) []

	       fun mkgr [] d u im = FGRAPH {control = gc, def = d, use = u, ismove = im}
             | mkgr ((n,OPER{src,dst, jump,...})::xs) d u im = let 
													val d' = Splaymap.insert(d,n,dst) (*aca estaba Splaymap.insert(d,n,src) *)
                                                    val u' = Splaymap.insert(u,n,src) (* y aca Splaymap.insert(d,n,dst) *)
                                                    val im' = Splaymap.insert(im,n,false) 
                                                    val _ = case jump of
										                NONE => if (null xs) then () else mk_edge{from = n, to = #1 (List.hd xs)} 
                                                      | SOME lj => List.app (fn j => mk_edge{from = n, to = tabSaca(j,tablab)}) lj          
                                             in mkgr xs d' u' im' end
              | mkgr ( (n,MOVE{src,dst, ...})::xs) d u im = let val d' = Splaymap.insert(d,n,[dst]) (*aca lo mismo*)
                                                                val u' = Splaymap.insert(u,n,[src])
                                                                val im' = Splaymap.insert(im,n,true)        
                                                             val _ = if (null xs) then () else mk_edge{from = n, to = #1 (List.hd xs)} 
                                                       in mkgr xs d' u' im' end
              | mkgr _ _ _ _ = raise Fail ("error mkgr!") (*los nodos del grafo de control corresponde a instr OPER o MOVE*)
            in (mkgr tabnode  (Splaymap.mkDict cmp) (Splaymap.mkDict cmp) (Splaymap.mkDict cmp), List.map (#1) tabnode) end
end

