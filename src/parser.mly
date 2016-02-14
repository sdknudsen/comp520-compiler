%{
  open Ast
%}

%start <Ast.ast> program

%%

program:
  | pkg=package decls=decl+ EOF { Prog(pkg, decls) }
  | error { Error.print_error $startpos "syntax error" }

package:
  | PACKAGE ID SEMICOLON { Pkg($2) }
  | PACKAGE error { Error.print_error $startpos "package identifier" }

decl:
  | vd=var_decl { vd }
  | td=type_decl { td }
  | fd=func_decl { fd }

var_decl:
  | VAR LPAREN var_stmt* RPAREN SEMICOLON { None }
  | VAR ID LBRACKET INT RBRACKET golite_type { None }
  | VAR ID LBRACKET RBRACKET golite_type { None }
  | VAR var_stmt { Some($2) }
  | VAR error { Error.print_error $startpos "error at variable declaration" }
  | ID COLONEQ expr SEMICOLON { None }
  | ID COLONEQ error { Error.print_error $startpos "error at variable declaration" }

var_stmt:
  | var_rec { Some(Var_decl($1)) }
  | id_rec { Some(Var_decl($1)) }

var_rec:
  | ID COMMA var_rec COMMA expr { ($1,$5,None)::$3 }
  | ID option(golite_type) ASSIGNMENT expr { [($1,$4,$2)] }

id_rec:
  | ID COMMA id_rec { ($1,None,None)::$3 }
  | ID golite_type { [($1,None,$2)] }

type_decl:
  | TYPE LPAREN type_stmt* RPAREN SEMICOLON { None }
  | TYPE type_stmt { None }
  | TYPE error { Error.print_error $startpos "error at type declaration" }

type_stmt:
  | ID STRUCT LBRACE identifiers golite_type RBRACE SEMICOLON { None }
  | ID golite_type { None }

func_decl:
  | FUNC ID LPAREN parameters RPAREN LBRACE stmt*
      RETURN SEMICOLON RBRACE SEMICOLON { Func_decl($2, $4, $7, None, None) }
  | FUNC ID LPAREN parameters RPAREN golite_type LBRACE stmt*
      RETURN ID SEMICOLON RBRACE SEMICOLON { Func_decl($2, $4, $8, Some($10), Some($6)) }
  | FUNC error { Error.print_error $startpos "error at function declaration" }

identifiers:
  | ids=separated_nonempty_list(COMMA, ID) { ids }

parameters:
  | params=separated_list(COMMA, param_expr) { params }

param_expr:
  | identifiers golite_type { None }

golite_type:
  | ID { $1 }
(*
  | T_INT { TInt }
  | T_FLOAT64 { TFloat }
  | T_BOOL { TBool }
  | T_RUNE { TRune }
  | T_STRING { TString }
*)

stmt:
  | assignment SEMICOLON { Some(Assign($1)) }
  | for_stmt SEMICOLON { $1 }
  | if_stmt SEMICOLON { $1 }
  | vd=var_decl { vd }
  | td=type_decl { td }
  | error { Error.print_error $startpos "error at statement" }
  (* need to support empty statement *)

stmts_block:
  | LBRACE stmt* RBRACE { $2 }

for_stmt:
  | FOR b=stmts_block     { For_stmt(None, b) }
  | FOR c=expr b=stmts_block    { For_stmt(Some(c, None), b) }
  | FOR i=assignment SEMICOLON c=expr SEMICOLON u=assignment
        b=stmts_block     { For_stmt(Some(c, Some((i,u))), b)}

if_stmt:
  | IF expr stmts_block     { Some(If_stmt($2, $3, None)) }
  | IF expr stmts_block ELSE stmts_block  { Some(If_stmt($2, $3, Some($5))) }
  | IF expr stmts_block ELSE if_stmt  { Some(If_stmt($2, $3, Some([$5]))) }

expr:
  | LPAREN e=expr RPAREN { e }
  | expr e_binop expr { Bexp($2, $1, $3) }
  | e_prefix_op expr  { Uexp($1, $2) }
  | INT     { ILit($1) }
  | FLOAT64     { FLit($1) }
  | BOOL     { BLit($1) }
  | RUNE     { RLit($1) }
  | STRING     { SLit($1) }
  | ID      { Iden($1) }
  | error { Error.print_error $startpos "error at expression" }

assignment:
  (*| identifiers ASSIGNMENT expr { None }*)
  | ID ASSIGNMENT expr { ([$1], [$3]) }
  | ID a_binop expr { [($1, Bexp($2, Iden($1), $3))] }
  | ID a_postfix    { [($1, Bexp($2, Iden($1), ILit(1)))] }

%inline e_binop:
  | PLUS  { Plus }
  | MINUS { Minus }
  | TIMES { Times }
  | DIV   { Div }

%inline e_prefix_op:
  | PLUS  { Pos }
  | MINUS { Neg }

%inline a_binop:
  | PLUSEQ  { Plus }
  | MINUSEQ { Minus }
  | TIMESEQ { Times }
  | DIVEQ   { Div }

%inline a_postfix:
  | INC { Plus }
  | DEC { Minus }

(*
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
*)