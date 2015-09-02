(* Etapa 6 : Codigo Intermedio -- Entrega 2 *)

Pasemos a considerar el código intermedio:

Necesitaremos guardar y mantener mucha información; a esta estructura la llamaremos el static frame, y cada función tiger deberá tener su frame.
Entre otras cosas tendrá info para las variables que usarán las funciones, etc. Con respecto a las variables, el valor de estas trataremos de que esté en un registro (las llamaremos temporarios, y tenemos cuantos querramos).

Siempre podemos hacer esto, excepto que:

1) La variable escapa. Debe estar en la memoria del frame.
2) La variable es un parámetro, y por la convención de llamada del procesador elegido, deba ir en memoria.

Definimo:

signature tigertemp = sig
	type label = string
	type temp = string
	val makeString: string -> string
	val newtemp: unit -> temp
	val temp2string: temp -> string
	val newlabel: unit -> label
	..
	val rv: temp (* return value *)
	val fp: temp
end


