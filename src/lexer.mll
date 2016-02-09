{
  open Lexing
  open Parser

  let insert_semic = ref false
}

(* from minilang *)
let eol = '\r' | '\n' | ('\r' '\n')
let int_lit = '0' | ['1'-'9'] ['0'-'9']*
let flt_lit = int_lit '.' ['0'-'9']* | '.' ['0'-'9']+
let str_lit = ['a'-'z' 'A'-'Z' '0'-'9' ' ' '.' '!' '?' ',']*
let iden = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

rule token = parse
  | '+'		{ insert_semic:=false; PLUS }
  | '-'		{ insert_semic:=false; MINUS }
  | '*'		{ insert_semic:=false; TIMES }
  | '/'		{ insert_semic:=false; DIV }
  | '%'		{ insert_semic:=false; PERCENT }
  | '&'		{ insert_semic:=false; BITAND }
  | '|'		{ insert_semic:=false; BITNOT }
  | '^'		{ insert_semic:=false; CIRCUMFLEX }
  | '<'		{ insert_semic:=false; LCHEVRON }
  | '>'		{ insert_semic:=false; RCHEVRON }
  | '!'		{ insert_semic:=false; BANG }
  | '='		{ insert_semic:=false; ASSIGNMENT }
  | '('		{ insert_semic:=false; LPAREN }
  | ')'		{ insert_semic:=true;  RPAREN }
  | '['		{ insert_semic:=false; LBRACKET }
  | ']'		{ insert_semic:=true;  RBRACKET }
  | '{'		{ insert_semic:=false; LBRACE }
  | '}'		{ insert_semic:=true;  RBRACE }
  | ','		{ insert_semic:=false; COMMA }
  | '.'		{ insert_semic:=false; DOT }
  | ';'		{ insert_semic:=false; SEMICOLON }
  | ':'		{ insert_semic:=false; COLON }
  | "<<"	{ insert_semic:=false; LSHIFT }
  | ">>"	{ insert_semic:=false; RSHIFT }
  | "&^"	{ insert_semic:=false; BITNAND }
  | "+="	{ insert_semic:=false; PLUSEQ }
  | "-="	{ insert_semic:=false; MINUSEQ }
  | "*="	{ insert_semic:=false; TIMESEQ }
  | "/="	{ insert_semic:=false; DIVEQ }
  | "%="	{ insert_semic:=false; PERCENTEQ }
  | "&="	{ insert_semic:=false; AMPEQ }
  | "|="	{ insert_semic:=false; BITOREQ }
  | "^="	{ insert_semic:=false; BITNOTEQ }
  | "<<="	{ insert_semic:=false; LSHIFTEQ }
  | ">>="	{ insert_semic:=false; RSHIFTEQ }
  | "&^="	{ insert_semic:=false; BITNANDEQ }
  | "&&"	{ insert_semic:=false; BOOL_AND }
  | "||"	{ insert_semic:=false; BOOL_OR }
  | "<-"	{ insert_semic:=false; LARROW }
  | "++"	{ insert_semic:=false; INC }
  | "--"	{ insert_semic:=false; DEC }
  | "=="	{ insert_semic:=false; EQUALS }
  | "!="	{ insert_semic:=false; NOTEQUALS }
  | "<="	{ insert_semic:=false; LTEQ }
  | ">="	{ insert_semic:=false; GTEQ }
  | ":="	{ insert_semic:=false; COLONEQ }
  | "..."	{ insert_semic:=false; ELLIPSIS }

(* golite keywords *)
  | "break"	{ insert_semic:=false; BREAK }
  | "case"	{ insert_semic:=false; CASE }
  | "chan"	{ insert_semic:=false; CHAN }
  | "const"	{ insert_semic:=false; CONST }
  | "continue"	{ insert_semic:=false; CONTINUE }
  | "default"	{ insert_semic:=false; DEFAULT }
  | "defer"	{ insert_semic:=false; DEFER }
  | "else"	{ insert_semic:=false; ELSE }
  | "fallthroug"	{ insert_semic:=false; FALLTHROUGH }
  | "for"	{ insert_semic:=false; FOR }
  | "func"	{ insert_semic:=false; FUNC }
  | "go"	{ insert_semic:=false; GO }
  | "goto"	{ insert_semic:=false; GOTO }
  | "if"	{ insert_semic:=false; IF }
  | "import"	{ insert_semic:=false; IMPORT }
  | "interface"	{ insert_semic:=false; INTERFACE}
  | "map"	{ insert_semic:=false; MAP }
  | "package"	{ insert_semic:=false; PACKAGE }
  | "range"	{ insert_semic:=false; RANGE }
  | "return"	{ insert_semic:=false; RETURN }
  | "select"	{ insert_semic:=false; SELECT }
  | "struct"	{ insert_semic:=false; STRUCT }
  | "switch"	{ insert_semic:=false; SWITCH }
  | "type"	{ insert_semic:=false; TYPE }
  | "var"	{ insert_semic:=false; VAR }
(*
  | "int"			{ insert_semic:=false; INT }
  | "float64"		{ insert_semic:=false; FLOAT64 }
  | "bool"		{ insert_semic:=false; BOOL }
  | "rune"		{ insert_semic:=false; RUNE }
  | "string"		{ insert_semic:=false; STRING }
  | "print"		{ insert_semic:=false; PRINT }
  | "println"		{ insert_semic:=false; PRINTLN }
  | "append"		{ insert_semic:=false; APPEND }
*)

(* from minilang *)
  | eof		{ EOF }

  | eol { 
      Lexing.new_line lexbuf;
      if !insert_semic
      then (insert_semic:=false; SEMICOLON)
      else (
        insert_semic:=false;
        token lexbuf
      )
    }

  | [' ' '\t']
    { token lexbuf }

  | "//" [^'\r''\n']*
    { token lexbuf } (* line comment *)

  | "/*" ( [^'*'] | "*"[^'/'])* "*/"
    { token lexbuf } (* block comment *)

  | flt_lit  as f
    { insert_semic:=true; FLOAT (float_of_string f) }

  | int_lit as d
    { insert_semic:=true; INT (int_of_string d) }

  | '"'(str_lit as s)'"'
    { insert_semic:=true; STR s }

  | iden as x
    { insert_semic:=true; ID x }

  | _ {
      insert_semic:=false;
      Error.print_error
        lexbuf.lex_curr_p
        (Printf.sprintf "unexpected token '%s'" (lexeme lexbuf))
    }
