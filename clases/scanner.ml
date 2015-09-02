(* Etapa 1 *)

Consideraciones sobre scanners y parsers

Definición:
	Dado un alfabeto finito A (ej:ASCII) se definen las expresiones regulares como:
	
1) Un caracter de A es una exp. regular.
2) Dadas 2 exp. reg, su concatenación es una exp. reg.
3) Dados 2 exp. reg, su alternación er1|er2 es una exp. reg.
4) Dada una exp. reg., er* (concatenación de er 0 o más veces) es una er.

Se agregan para simplicidad:

5) er+, repetición de 1 o más veces.
6) [c1c2...cn] , ci pertenece a A == c1 | c2 | .. | cn
Si son consecutivos, [c1c2...cn] == [c1-cn]
7) Negación [^c1..cn] = Cualquier caracter, excepto c1 .. cn
8) . == Cualquier carácter

Existen varios programas, para tomar er y generar uno de los AF correspondientes.
Ej: Una er para números enteros positivos en decimal
a) [0-9]+
b) [1-9][0-9]* | 0

Especificación de un scanner para mosmllex

Nota: Un scanner debe:

* Producir tokens
* Eliminar espacios innecesarios y comentarios
* Contar cuántas líneas hemos leído
* Procesar las strings cttes

Estos requerimientos traen complicaciones.

Ej: Un espacio { Ignorado --> En un comentario o en el programa
			     Debe ser tomado en cuenta.  }
			   
    Materia de mierda { Ignorado --> En un comentario
			            Error -> En el programa  }
			            
			          
Mosmllex y otros permiten especificar varios scanners, lo que permite resolver lo anterior.

{ 
	Codigo ML
}
definiciones (nombre a er.largas)

rule Nombre parse
     er1 {accion 1}
   | er2 {accion 2}
   | ern {accion n}
   
and Nombre parse
	er1' 
	...

El código generado brinda una función getLexeme : lexbuf -> String que nos da la string concreta que coincidio con la er.

Bosquejemos un scanner p/tiger

rule Lexer parse
	[' ''\t''\r']+ 	{Lexer lexbuf}
|	'\n'			{Cuenta los \n}
|	"/*"			{Coment lexbuf; Lexer lexbuf}
|	eof				{EOF}
|	'"'				{TEXTO(String lexbuf)}

/* Scanner para coment */
and Coment = parse
	eof		{raise Faiil "Comentario no cerrado"}
|	"/*"	{incComent()}
|	"*/"	{IF decComent() = 0 then () else Coment lexbuf}
|	'\n'	{Cuenta los \n}
|	_		{Coment lexbuf}

/* Scanner para string */
and String = parse
	eof		{raise Faiil "String no cerrada"}
|	'"'		{""} (*DUDA: Que es esto?*)
|	"\\\\"	{"\\" ^ String lexbuf}
|	"\\n"	{"\n" ^ String lexbuf} 
|	'\\'	{String1 lexbuf}
|	"\\\""	{"\"" ^ String lexbuf}
| 	['@'-'z']	{getLexeme lexbuf ^ String lexbuf}
|	_		{raise Fail (getLexeme lexbuf ^ "prohibido"}

and String1 = parse   (* abc\    \def  === abcdef *) 
	eof		{raise Faiil "String no cerrada"}
|	[' ''\t''\r']+ 	{String1 lexbuf}
|	'\n'	{Cuenta los \n}
|	"\\"	{()}
|	_		{raise Fail "error en String"}


Es perfectamente posible hacer esto:
|	"for"	{FOR}
|	"var"	{VAR}
|	"array"	{ARRAY}
	...
	
Pero usando el hecho de que las palabras claves de tiger empiezan en minúscula, hacer esto:
|	['a'-'z']+	{idPalClave (getLexeme lexbuf)}

Y en la parte con código ML

val tabla = Polyhash.mkPolyTable (50,Empty)
val _ = List.app
				(Fn (k,v) => Polyhash.insert (k,v) tabla)
				[("array", ARRAY)
				|("for",FOR)
				...

fun idPalClave s = case Polyhash.peek (tabla,s) OF
						SOME x => x
					|	NONE => ID s
