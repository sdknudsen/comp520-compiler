{
  open Lexing
  open Parser
}

(* from minilang *)
let eol = '\r' | '\n' | ('\r' '\n')
let int_lit = '0' | ['1'-'9'] ['0'-'9']*
let flt_lit = int_lit '.' ['0'-'9']* | '.' ['0'-'9']+
let str_lit = ['a'-'z' 'A'-'Z' '0'-'9' ' ' '.' '!' '?' ',']*
let iden = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

rule token = parse
(* golite operators *)
  | '+'   { PLUS }
  | '-'   { MINUS }
  | '*'   { TIMES }
  | '/'   { DIV }
(*  | '%'   {  }
  | '&'   {  }
  | '|'   {  }
  | '^'   {  }
  | '<'   {  }
  | '>'   {  }
  | '!'   {  }
*)
  | '='   { ASSIGNMENT }
  | '('   { LPAREN }
  | ')'   { RPAREN }
(*
  | '['   {  }
  | ']'   {  }
  | '{'   {  }
  | '}'   {  }
  | ','   {  }
  | '.'   {  }
*)
  | ';'   { SEMICOLON }
  | ':'   { COLON }

(*  | "<<"    {  }
  | ">>"    {  }
  | "&^"    {  }
  | "+="    {  }
  | "-="    {  }
  | "*="    {  }
  | "/="    {  }
  | "%="    {  }
  | "&="    {  }
  | "|="    {  }
  | "^="    {  }
  | "<<="   {  }
  | ">>="   {  }
  | "&^="   {  }
  | "&&"    {  }
  | "||"    {  }
  | "<-"    {  }
  | "++"    {  }
  | "--"    {  }
  | "=="    {  }
  | "!="    {  }
  | "<="    {  }
  | ">="    {  }
  | ":="    {  }
  | "..."   {  }
*)


(* golite keywords *)
(*
  | "break"   { BREAK }
  | "case"    { CASE }
  | "chan"    { CHAN }
  | "const"   { CONST }
  | "continue"    { CONTINUE }
  | "default"   { DEFAULT }
  | "defer"   { DEFER }
  | "else"    { ELSE }
  | "fallthrough"   { FALLTHROUGH }
  | "for"     { FOR }
  | "func"    { FUNC }
  | "go"      { GO }
  | "goto"    { GOTO }
  | "if"      { IF }
  | "import"    { IMPORT }
  | "interface"   { INTERFACE}
  | "map"     { MAP }
  | "package"   { PACKAGE }
  | "range"   { RANGE }
  | "return"    { RETURN }
  | "select"    { SELECT }
  | "struct"    { STRUCT }
  | "switch"    { SWITCH }
  | "type"    { TYPE }
  | "var"     { VAR }
  
  | "int"     { INT }
  | "float64"   { FLOAT64 }
  | "bool"    { BOOL }
  | "rune"    { RUNE }
  | "string"    { STRING }
  | "print"   { PRINT }
  | "println"   { PRINTLN }
  | "append"    { APPEND }
*)

(* from minilang *)
  | "var"    { VAR }
  | "int"    { T_INT }
  | "float"  { T_FLOAT }
  | "string" { T_STRING }

  | "print"  { PRINT }
  | "read"   { READ }

  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "endif"  { ENDIF }

  | "while"  { WHILE }
  | "do"     { DO }
  | "done"   { DONE }

  | eof      { EOF }
  | eol      { new_line lexbuf; token lexbuf } (* SEMICOLON? *)

  | [' ''\t']                        { token lexbuf }
  | "//" [^'\r''\n']*                { token lexbuf } (* line comment *)
  | "/*" ( [^'*'] | "*"[^'/'])* "*/" { token lexbuf } (* block comment *)

  | int_lit as n         { INT (int_of_string n) }
  | flt_lit as f         { FLOAT (float_of_string f) }
  | '"'(str_lit as s)'"' { STRING s }
  | iden as x            { ID x }

  | _ { Error.print_error lexbuf.lex_curr_p (Printf.sprintf "unexpected token '%s'" (lexeme lexbuf)) }
