{
open Parser
exception LexError of Lexing.position
}

(* from minilang *)
let eol = ('\r' '\n') | '\n' | '\r' 
let int_lit = '0' | ['1'-'9'] ['0'-'9']*
let flt_lit = int_lit '.' ['0'-'9']* | '.' ['0'-'9']+
let str_lit = ['a'-'z' 'A'-'Z' '0'-'9' ' ' '.' '!' '?' ',']*
let iden = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*


rule lex = parse
(* golite operators *)
	| '+'		{ PLUS }
	| '-'		{ MINUS }
	| '*'		{ TIMES }
	| '/'		{ DIV }
(*	| '%'		{  }
	| '&'		{  }
	| '|'		{  }
	| '^'		{  }
	| '<'		{  }
	| '>'		{  }
	| '!'		{  }
*)
	| '='		{ ASSIGNMENT }
	| '('		{ LPAREN }
	| ')'		{ RPAREN }
(*
	| '['		{  }
	| ']'		{  }
	| '{'		{  }
	| '}'		{  }
	| ','		{  }
	| '.'		{  }
*)
	| ';'		{ SEMICOLON }
	| ':'		{ COLON }

(*	| "<<"		{  }
	| ">>"		{  }
	| "&^"		{  }
	| "+="		{  }
	| "-="		{  }
	| "*="		{  }
	| "/="		{  }
	| "%="		{  }
	| "&="		{  }
	| "|="		{  }
	| "^="		{  }
	| "<<="		{  }
	| ">>="		{  }
	| "&^="		{  }
	| "&&"		{  }
	| "||"		{  }
	| "<-"		{  }
	| "++"		{  }
	| "--"		{  }
	| "=="		{  }
	| "!="		{  }
	| "<="		{  }
	| ">="		{  }
	| ":="		{  }
	| "..."		{  }
*)


(* golite keywords *)
(*
	| "break"		{ BREAK }
	| "case"		{ CASE }
	| "chan"		{ CHAN }
	| "const"		{ CONST }
	| "continue"		{ CONTINUE }
	| "default"		{ DEFAULT }
	| "defer"		{ DEFER }
	| "else"		{ ELSE }
	| "fallthrough"		{ FALLTHROUGH }
	| "for"			{ FOR }
	| "func"		{ FUNC }
	| "go"			{ GO }
	| "goto"		{ GOTO }
	| "if"			{ IF }
	| "import"		{ IMPORT }
	| "interface"		{ INTERFACE}
	| "map"			{ MAP }
	| "package"		{ PACKAGE }
	| "range"		{ RANGE }
	| "return"		{ RETURN }
	| "select"		{ SELECT }
	| "struct"		{ STRUCT }
	| "switch"		{ SWITCH }
	| "type"		{ TYPE }
	| "var"			{ VAR }
	
	| "int"			{ INT }
	| "float64"		{ FLOAT64 }
	| "bool"		{ BOOL }
	| "rune"		{ RUNE }
	| "string"		{ STRING }
	| "print"		{ PRINT }
	| "println"		{ PRINTLN }
	| "append"		{ APPEND }
*)

(* from minilang *)
	| "var"		{ VAR }
	| "int"		{ T_INT }
	| "float"	{ T_FLOAT }
	| "string"	{ T_STRING }

	| "print"	{ PRINT }
	| "read"	{ READ }

	| "if"		{ IF }
	| "then"	{ THEN }
	| "else"	{ ELSE }
	| "endif"	{ ENDIF }

	| "while"	{ WHILE }
	| "do"		{ DO }
	| "done"	{ DONE }

	| eof		{ EOF }
	| eol		{ Lexing.new_line lexbuf; lex lexbuf } (* SEMICOLON? *)

	| [' ' '\t']		{ lex lexbuf }
	| ('#' [^ '\n' '\r']* )	{ lex lexbuf } 

	| flt_lit  as f		{ FLOAT (float_of_string f) }
	| int_lit as d		{ INT (int_of_string d) }
	| '"'(str_lit as s)'"'	{ STRING s }
	| iden as x		{ ID x }
	| _			{ raise (LexError (Lexing.(lexbuf.lex_curr_p))) }
