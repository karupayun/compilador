structure tigerabs = 
struct

type symbol = string
type pos = int

datatype var = SimpleVar of symbol (* se encontro una variable suelta Ej aa := expresion              *)
	| FieldVar of var * symbol     (* acceso a un record aaa.x  aaa -> es var y x -> symbol           *)
	| SubscriptVar of var * exp    (* acceso a los arreglos aaa[expint] aaa -> es var y expint -> exp *)

(* and es pq los tipos son mutuamente recursivos *)
(* pos se usa para llevar el numero de linea para mostrar errores *)
and exp = VarExp of var * pos                                           (* variable como valor *) 
	| UnitExp of pos                                                    (* ()                  *)
	| NilExp of pos                                                     (* nil                 *)
	| IntExp of int * pos                                               (* 22                  *)
	| StringExp of string * pos                                         (* "22"                *)
	| CallExp of {func: symbol, args: exp list} * pos                   (* func -> nombre de funcion args-> argumentos *)
	| OpExp of {left: exp, oper: oper, right: exp} * pos
(* b>a OpExp{left=Varexp(SimpleVar "b"), oper=GtOp, right=VarExp(SimpleVar "a")}  *)
	| RecordExp of {fields: (symbol * exp) list, typ: symbol} * pos
(* R{a=7, b="hola"}  <---> RecordExp{fields=[("a",IntExp 7),("b",StringExp "hola")], typ="R"}   *)
	| SeqExp of exp list * pos
(* 10 ; 5 *)
	| AssignExp of {var: var, exp: exp} * pos
(* var := exp *)
	| IfExp of {test: exp, then': exp, else': exp option} * pos
(* if test then then' else (else')  (option es como maybe -> NONE o SOME exp) *)
	| WhileExp of {test: exp, body: exp} * pos
	| ForExp of {var: symbol, escape: bool ref,
		     lo: exp, hi: exp, body: exp} * pos
(* for a:=10 to 20 do ... var=a lo=10 hi=20 body= ... 
   escape es referencia para completar a posteriori. Se hace un AST donde las referencias quedan vacías y en la etapa de escape se completa
   escape -> indica si es una variable escapada *)
	| LetExp of {decs: dec list, body: exp} * pos
(* let
      decs
   in
      body
   end
*)
	| BreakExp of pos
	| ArrayExp of {typ: symbol, size: exp, init: exp} * pos
(* int[10] of 5 typ -> int, size -> 10, init -> 5 *)

and dec = FunctionDec of ({name: symbol, params: field list,
		result: symbol option, body: exp} * pos) list
	| VarDec of {name: symbol, escape: bool ref,
		     typ: symbol option, init: exp} * pos
	| TypeDec of ({name: symbol, ty: ty} * pos) list
(* le asigna name a ty 
   pq una lista? para tipos mutuamente recursivos se hacía en bloques, cada bloque es un TypeDec 
   como se delimita cada bloque? son conjuntos maximales de definiciones de tipos sucesivas
   
let
    type a = int
    type b = {x:int, y:string}
    type c = array of a
    ...

Con las declaraciones de funciones pasa lo mismo (se hacen lista dentro de FunctionDec para cada bloque)
la recursividad mutua solo se permite dentro del bloque.

function f(a:int, b:string, c:R) : int = ...
name -> f   params -> (a:int, b:string, c:R)      result -> int     body = ...
*)


and ty = NameTy of symbol
	| RecordTy of field list
	| ArrayTy of symbol
and oper = PlusOp | MinusOp | TimesOp | DivideOp
	| EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

withtype field = {name: symbol, escape: bool ref, typ: ty}
end
