(* Etapa 2 *)

Parser

Se especifica usando una gramática libre de contexto, del subconjunto Left Recursive, LALR (Look Ahea Left Recursive).


Comentario de la especificación: /*

% {
	Codigo ML
% }
	directivas, definiciones, etc
%%
	reglas de producción
%%
	más código ML
	
Bosquejo para tiger y problemas que saldrán:

En las definiciones:
/* los terminales */
% token ARRAY .. FOR
% token <int> NRO
% token <string> ID TEXTO
% token MAS MENOS FOR
% token EOF

Empecemos con las reglas de producción:

% start prog
%%
prog: expr EOF
	;

expr : id
	| expr MAS expr
	| expr POR expr
	
% left MAS MENOS
% left POR DIV
% Nonassoc MENOR


