structure tigersres =
struct

open tigerabs
open tigertab
open tigertips

datatype EnvEntry =
	 Var of {ty: Tipo, access: tigertrans.access, level: int}
	| Func of {level: tigertrans.level, label: tigertemp.label,
		formals: Tipo list, result: Tipo, extern: bool}

end
