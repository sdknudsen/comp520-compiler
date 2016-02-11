%{
  open Ast
%}

%start <Ast.ast> program

%%

program:
  stmt* EOF { Prog($1) }
;

stmts_block:
  LBRACE stmt* RBRACE { $2 }
;

(*
decl:
  VAR decl_line
| VAR LPAREN decl_line* RPAREN
| FUNC ID LPARENS RPARENS
;

decl_line:
  ID ID* ID
| ID EQUAL expr
| ID ID EQUAL expr
;

*)

for_stmt:
  FOR b=stmts_block     { For_stmt(None, b) }
| FOR c=expr b=stmts_block    { For_stmt(Some(c, None), b) }
| FOR i=assignment SEMICOLON c=expr SEMICOLON u=assignment
    b=stmts_block     { For_stmt(Some(c, Some((i,u))), b)}
;


(*
  TODO: maybe we should revisit how the ast implements If-stmt. I find it weird
  that we must enclose the if statement in a list. Maybe use a `either` type?
*)
if_stmt:
  IF expr stmts_block     { If_stmt($2, $3, None) }
| IF expr stmts_block ELSE stmts_block  { If_stmt($2, $3, Some($5)) }
| IF expr stmts_block ELSE if_stmt  { If_stmt($2, $3, Some([$5])) }
;


(*
  TODO: Decide if print is a statement or if we handle it as a function call.
  Note that if we implement it as a function call, we nee to handle multiple
  parameters.
*)
stmt:
  assignment SEMICOLON      { Assign($1) }
(* | PRINT LPAREN expr* RPAREN SEMICOLON  { Print($2) } *)
| for_stmt SEMICOLON      { $1 }
| if_stmt SEMICOLON     { $1 }
| SEMICOLON       { Empty }

(*| error { raise (ParseError($startpos,"statement")) }*)
;

%inline e_binop:
  PLUS  { Plus }
| MINUS { Minus }
| TIMES { Times }
| DIV { Div }
;

%inline e_prefix_op:
| PLUS  { Pos }
| MINUS { Neg }
;

expr:
  INT     { ILit($1) }
| FLOAT64     { FLit($1) }
| BOOL     { BLit($1) }
| RUNE     { RLit($1) }
| STRING     { SLit($1) }
| ID      { Iden($1) }
| LPAREN expr RPAREN  { $2 }
| expr e_binop expr { Bexp($2, $1, $3) }
| e_prefix_op expr  { Uexp($1, $2) }

(*| error { raise (ParseError($startpos,"expression")) }*)
;


%inline a_binop:
  TIMESEQ { Times }
| DIVEQ   { Div }
| PLUSEQ  { Plus }
| MINUSEQ { Minus }
;

%inline a_postfix:
  INC { Plus }
| DEC { Minus }
;

(*
assignment_rec:
  ID COMMA assignment_rec COMMA expr    { ($1::(fst $2), $3::(snd $2)) } 
| ID ASSIGNMENT expr        { ([$1], [$2]) }
;
*)

(*
  TODO: Handle multiple assignements
*)
assignment:
(*  assignment_rec  { (fst $1, rev (snd $1)) }
| *)
  ID a_binop expr { [($1, Bexp($2, Iden($1), $3))] }
| ID a_postfix    { [($1, Bexp($2, Iden($1), ILit(1)))] }
;
