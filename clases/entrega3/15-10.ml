(*
Entrega 3 - Clase 15/10

Una base para hacer un DSL para iteraciones.
Encontrado entre la documentacion de MLTON

*)

datatype ('a,'b) product = & of 'a*'b
infix &
signature ITER = 
sig 
    type 'a t = ('a -> unit) -> unit
    val return = 'a -> 'a t
    val >>== 'a t * ('a -> 'b t) -> 'b t
    val non0 inte = 'a t
    val to = int * int -> int t
    val downto : int * int -> int t
    val inList : 'a list -> 'a t
    val inVector : 'a vector -> 'a t
    val inArray  'a array -> 'a t
    val inSet :'a Splayset.set -> ' a t
    val inDict : ('a,'b) Splaymap.dict -> ('a*'b) t
    val using : ('a,'b) StringCvt.reader
    val when : 'a t * ('a -> bool) -> 'a t
    val by : 'a t * ('a -> 'b) -> 'b t
    val @@ : 'a t * 'a t -> 'a t
    val ** : 'a t *'b t -> ('a,'b) product
    val for: 'a -> 'a
end
(* varios de estos combinadores se usarán infijos *)

infix 2 to downto
infix 1 @@ when by
infix 0 >>= **
(*se usan estos combinadores usuales *)
fun const x _= x
fun flip f x y = f y x
fun id x = x
fun opt fno fso = fn NONE => fno () | SOME ? => fso ?
fun pass x f  f x
infix ls rs
fun (x ls f) y = fs(x,y)
fun (f rs y) x = f (x,y)

structure Iter :> ITER = 
struct
    type 'a t = ('a -> unit) -> unit
    val return = pass
    fun (iA >>= a2iB) f = iA (flip a2ib f)
    val none = ignore (* 'a -> unit *)
    fun (L to n) f = let fun loop = if L < n then (f L;loop (L+1)) else () in loop L end
    fun (n downto L) f = let fun loop n = if n > L then (f n;loop (n-1) else () in loop n end
    fun inList ? = flip List.app ?
    fun inVector ? = flip Vector.app ?
    fun inArray ? = flipArray.app ?
    fun inSet ? = flip Splayset.app ?
    fun inDict ? = flip Splaymap.app ?
    fun using get s f = 
            let fun loop s = opt (const ()) (fn (x,s) => (f x;loop s)) (get s)
            in loop s end
    fun (iA when p) f = iA (fn a => if p a then f a else 0)
    fun (iA by g) f = iA(f o g)
    fun (iA @@ iB) f = (iA f:unit;iB f)
    fun (iA ** iB) f = fn a => iB(fn b => f(a&b))
    val for : id
end

(* dos ejemplos de uso *)
openIter
infix 2 to downto
infix 1 @@ when by
infix 0 >>= **

val _ = for (0 to 10 when (fn x => x mod 3 <> 0)**
            inList ["a","b"] ** 2 downto 1 by real)
        (fn x & y & z => print("("^Int.toString x^",\""^y^","^Real.toString z^")\n"))
(* ej. usando un conjunto*)
fun setfromList L = Splayset.addList (Splayset.empty String.compare,L)
val _  for (0 to 10 when((op<>rs 0) o (op mod rs 3)) ** inSet (setfromList ["a","b"])
            ** 2 downto 1 by real)
          (fn x & y & z => ......)
          
(* sigamos con las funciones del coloreo *)
 procedure Coalesce ()
    let m(=copy(x,y)) ∈ workListMoves
    x <- GetAlias(x)
    y <- GetAlias(y)
    if y ∈ precolored then
        let (u,v) = (y,x)
    else
        let (u,v) = (x,y)
    workListMoves <-workListMoves \ {m}
    if (u = v) then
        coalescedMoves <- coalescedMoves U {m}
        addWorkList(u)
    else if (v ∈ precolored or (u,v) ∈ adjSet) then
        constrainedMoves <- constrainedMoves U {m}
        addWorkList (u)
        addWorkList (v)        
    else if ((u ∈ precolored and (pt t ∈ adjacent(v), OK (t,u))) or (n ∉ precolored and
        Conservative (Adjacent (u) U Adjacent(v))) then
            coalescedMoves <- coalescedMoves U {m}
            Combine (u,v)
            addWorkList (u)
    else
        activeMoves <- activeMoves U {m}

procedure AddWorkList (u)
    if (u ∉ precolored and (not MoveRelated (u)) and (degree[u] < K)) then
        freezeWorkList <- freezeWorkList \ {u}
        simplifyWorkList <- simplifyWorkList U {u}
        
function OK (t,r)
    degree[t] < K or t ∈ precolored or (t,r) ∈ adjSet
    
function Conservative(nodes)
    let k = 0
    forall n ∈ nodes
        if degree[u] >= K then k <- k+1      
    return (k < K)

function getAlias(n)
    if n ∈ coalescedNodes then
        GetAlias (alias[n])
    else n
    
         
