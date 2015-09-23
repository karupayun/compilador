structure tigertips =
struct

type unique = unit ref
datatype Tipo = TUnit
	| TNil
	| TInt of read
	| TString
	| TArray of Tipo ref * unique
	| TRecord of (string * Tipo ref * int) list * unique
	| TTipo of string
and read = RO | RW

end
