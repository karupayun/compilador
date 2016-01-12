structure tigerflow :> tigerflow =
struct
    open tigergraph
    open tigerassem
    open tigertab
    type instr = tigerassem.instr
    type temp = tigertemp.temp

    datatype flowgraph = FGRAPH of {control: graph,
				    def: (node, temp list) Splaymap.dict,
				    use: (node, temp list) Splaymap.dict,
				    ismove: (node, bool) Splaymap.dict}

(*  datatype instr = OPER of {assem:string, src:temp list, dst:temp list, jump:label list option }
               | LABEL of {assem:string, lab:label}
               | MOVE of {assem:string, src:temp, dst:temp}*)



(*
Marga:     datatype flowgraph = FGRAPH of {control: tigergraph.graph,
                                    def: (tigergraph.node * tigertemp.temp list) list,
                                    use: (tigergraph.node * tigertemp.temp list) list,
                                    ismove: (tigergraph.node * bool) list}

no se como crear tablas donde la clave sean de tipo node, creo que el problema esta en que hay un array pero no estoy segura. asi que cambia 	    def: tigertemp.temp list tigergraph.mapnode,
			    use: tigertemp.temp list tigergraph.mapnode,
			    ismove: bool tigergraph.mapnode
a
                def: tigergraph.node * tigertemp.temp list,
			    use: tigergraph.node * tigertemp.temp list,
			    ismove: tigergraph.node * bool
*)  
	(*MakeGraph va a venir acá para no hacer un modulo unitario *)
	fun fst (x,_) = x

    fun makeGraph lInstr = 
        let val gc = newGraph()
            (* mktln crea: * Los nodos del grafo de control y los asocia con la instr correspondiente (tn)  
                           * Una tabla que mapea cada label con el nodo al que corresponde (tl) *)
            fun mktln [] tn tl = (tn,tl) 
              | mktln (LABEL{assem, lab}::inss) tn tl = let val (tn', tl') = mktln inss [] (tabNueva()) 
                                                            val tn_new = tn@tn'
                                                            val nprox = case tn' of
                                                                      [] => raise Fail ("Error: lab sin instrucción posterior!")
                                                                    | ((n,_)::ns) => n          
                                                            val tl'' = tabInserta (lab, nprox, tl) (* Lo anterior + nueva lab *) 
                                                            val tl_new = tabInserList (tl'', (tabAList tl')) (* Las labs posteriores *)
                                                        in (tn_new, tl_new) end       
               | mktln (i::inss) tn tl = mktln inss (tn @ [(newNode(gc), i)] ) tl

            val (tabnode, tablab) = mktln lInstr [] (tabNueva(): (string, node) Tabla) 

            (* mkgr completa las estructuras necesarias, usando las tablab para asociar jumps directamente a nodos *)
            fun mkgr [] d u im = FGRAPH {control = gc, def = d, use = u, ismove = im}
              | mkgr ((n,OPER{src, dst, jump,...})::xs) d u im = let val d' = Splaymap.insert(d,n,src)(*d @ [(n,src)]*)
                                                                     val u' = Splaymap.insert(u,n,dst)(*u @ [(n, dst)]*)
                                                                     val im' = Splaymap.insert(im,n,false)(*im @ [(n, false)]*)         
                                                                     val _ = case jump of
                                                                             NONE => if (null xs) then () else mk_edge{from = n, to = fst (List.hd xs)} 
                                                                           | SOME lj => List.app (fn j => mk_edge{from = n, to = tabSaca(j,tablab)} ) lj   
                                                            in mkgr xs d' u' im' end
              | mkgr ( (n,MOVE{src,dst, ...})::xs) d u im = let val d' = Splaymap.insert(d,n,[src])(*d @ [(n,[src])]*)
                                                                val u' = Splaymap.insert(u,n,[dst])(*u @ [(n, [dst])]*)
                                                                val im' = Splaymap.insert(im,n,true)(*im @ [(n, true)]*)        
                                                             val _ = if (null xs) then () else mk_edge{from = n, to = fst (List.hd xs)}
                                                       in mkgr xs d' u' im' end
              | mkgr _ _ _ _ = raise Fail ("error mkgr!") (*los nodos del grafo de control corresponde a instr OPER o MOVE*)
            in (mkgr tabnode  (Splaymap.mkDict cmp) (Splaymap.mkDict cmp) (Splaymap.mkDict cmp)(*[] [] []*),List.map (fst) tabnode) end
end
