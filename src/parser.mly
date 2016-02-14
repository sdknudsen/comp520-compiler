%{
  open Ast
%}

%start <Ast.ast> program

%%

program:
  | pkg=package decls=decl+ EOF { Prog(pkg, decls) }
  | error { Error.print_error $startpos "syntax error" }

package:
  | PACKAGE pkg_id=IDEN SEMICOLON { Pkg(pkg_id) }
  | PACKAGE error { Error.print_error $startpos "package identifier" }

decl:
  | vd=var_decl { vd }
  | td=type_decl { td }
  | fd=func_decl { fd }

var_decl:
  | VAR LPAREN vss=var_stmts RPAREN SEMICOLON { vss }
  | VAR vs=var_stmt { vs }
  | VAR error { Error.print_error $startpos "error at variable declaration" }
  | var_id=IDEN COLONEQ e=expr SEMICOLON { Var_decl([(var_id, Some(e), None)]) }
  | IDEN COLONEQ error { Error.print_error $startpos "error at variable declaration" }

var_stmts:
  | vss=var_stmts vs=var_stmt { ignore(vss); vs }
  | vs=var_stmt { vs }

(*
var_stmt:
  | identifiers typ_id=IDEN ASSIGNMENT separated_list(COMMA, expr) SEMICOLON {None}
  | identifiers ASSIGNMENT separated_list(COMMA, expr) SEMICOLON {None}
  | identifiers typ_id=IDEN { None }
*)

var_stmt:
  | vrec1=var_rec1 { Var_decl(vrec1) }
  | vrec2=var_rec2 { Var_decl(vrec2) }
  (*
  | var_id=IDEN LBRACKET INT RBRACKET typ_id=IDEN { None }
  | var_id=IDEN LBRACKET RBRACKET typ_id=IDEN { None }
  *)

var_rec1:
  | var_id=IDEN COMMA next=var_rec1 { (var_id, None, None) :: next }
  | var_id=IDEN typ_id=IDEN { [(var_id, None, Some(typ_id))] }

var_rec2:
  | var_id=IDEN COMMA next=var_rec2 COMMA e=expr { (var_id, Some(e), None) :: next }
  | var_id=IDEN typ_id=option(IDEN) ASSIGNMENT e=expr { [(var_id, Some(e), typ_id)] }

type_decl:
  | TYPE LPAREN tss=type_stmts RPAREN SEMICOLON { tss }
  | TYPE ts=type_stmt { ts }
  | TYPE error { Error.print_error $startpos "error at type declaration" }

type_stmts:
  | tss=type_stmts ts=type_stmt { ignore(tss); ts }
  | ts=type_stmt { ts }

type_stmt:
  | var_id=IDEN typ_id=IDEN { Type_decl(var_id, typ_id) }
  | var_id=IDEN STRUCT LBRACE var_ids=identifiers typ_id=IDEN RBRACE SEMICOLON
      { Struct_decl(var_id, var_ids, typ_id) }

identifiers:
  | ids=separated_nonempty_list(COMMA, IDEN) { ids }

func_decl:
  | FUNC fun_id=IDEN LPAREN params=parameters RPAREN
      LBRACE stmts=stmt* RETURN SEMICOLON RBRACE SEMICOLON
      { Func_decl(fun_id, params, stmts, None, None) }
  | FUNC fun_id=IDEN LPAREN params=parameters RPAREN typ_id=IDEN
      LBRACE stmts=stmt* RETURN var_id=IDEN SEMICOLON RBRACE SEMICOLON
      { Func_decl(fun_id, params, stmts, Some(var_id), Some(typ_id)) }

parameters:
  | params=separated_list(COMMA, param_expr) { params }

param_expr:
  | var_ids=identifiers typ_id=IDEN { (var_ids, typ_id) }

stmts_block:
  | LBRACE stmts=stmt* RBRACE { stmts }

for_stmt:
  | FOR b=stmts_block     { For_stmt(None, b) }
  | FOR c=expr b=stmts_block    { For_stmt(Some(c, None), b) }
  | FOR i=assignment SEMICOLON c=expr SEMICOLON u=assignment
        b=stmts_block     { For_stmt(Some(c, Some((i,u))), b)}

if_stmt:
  | IF expr stmts_block     { If_stmt($2, $3, None) }
  | IF expr stmts_block ELSE stmts_block  { If_stmt($2, $3, Some($5)) }
  | IF expr stmts_block ELSE if_stmt  { If_stmt($2, $3, Some([$5])) }

stmt:
  | assignment SEMICOLON      { Assign($1) }
  | for_stmt SEMICOLON      { $1 }
  | if_stmt SEMICOLON     { $1 }
(*
  | vd=var_decl { vd }
  | td=type_decl { td }
  | error { Error.print_error $startpos "error at statement" }
  (* need to support empty statement *)
*)

%inline e_binop:
  | PLUS  { Plus }
  | MINUS { Minus }
  | TIMES { Times }
  | DIV { Div }

%inline e_prefix_op:
  | PLUS  { Pos }
  | MINUS { Neg }

expr:
  | INT     { ILit($1) }
  | FLOAT64     { FLit($1) }
  | BOOL     { BLit($1) }
  | RUNE     { RLit($1) }
  | STRING     { SLit($1) }
  | IDEN      { Iden($1) }
  | LPAREN expr RPAREN  { $2 }
  | expr e_binop expr { Bexp($2, $1, $3) }
  | e_prefix_op expr  { Uexp($1, $2) }

%inline a_binop:
  | TIMESEQ { Times }
  | DIVEQ   { Div }
  | PLUSEQ  { Plus }
  | MINUSEQ { Minus }

%inline a_postfix:
  | INC { Plus }
  | DEC { Minus }

assignment:
  | IDEN a_binop expr { [($1, Bexp($2, Iden($1), $3))] }
  | IDEN a_postfix    { [($1, Bexp($2, Iden($1), ILit(1)))] }



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