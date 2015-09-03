open tigertips
open tigertab

fun ppTipo t = print (printTipo t^" ")
and printTipo TUnit = "Unit"
|   printTipo TNil = "Nil"
|   printTipo (TInt RW) = "Int RW"
|   printTipo (TInt _) = "Int RO"
|   printTipo TString = "String"
|   printTipo (TArray (t,_)) = "["^printTipo(t)^"]"
|   printTipo (TRecord (l,_)) = let val visitorFunc = fn ((name,ty,pos),x) => x^", "^name^"="^printTipo(ty)
                                    val recordContenido = String.extract(foldl visitorFunc "" l,2,NONE)
                                in "{"^ recordContenido ^"}" end
|   printTipo (TTipo (s,_))   = s

fun ppTab (aPrint:'a->unit) (bPrint:'b->unit) tab = let val l = tabAList tab
                                                        val f = fn ( (a,b),_) => ( aPrint a; print("=") ; bPrint b; print(" ") )
                                                        in foldl f () l end
                                  

val tabla = tabInserta("mariano", 22, tabInserta("pablo", 33, tabNueva()))
val _ = ppTab (fn x => print(x)) (fn x => print(Int.toString x)) tabla
(*val _ = ppTipo (TRecord ( [("pablo", TNil,11)  , ("mariano", TInt RW,13) , ("mauricio", TString,14)]  ,ref ())); *)
val _ = print("\n")
