%{
  open Ast
%}

%start <Ast.ast> program

%%

program:
| pkg=package decls=decl+ EOF
    { Prog(pkg, decls) }
| error
    { Error.print_error $startpos "syntax error" }

package:
| PACKAGE pkg_id=IDEN SEMICOLON
    { Pkg(pkg_id) }
| PACKAGE error 
    { Error.print_error $startpos "package identifier" }

decl:
| vd=var_decl
    { vd }
| td=type_decl
    { td }
| fd=func_decl
    { fd }

var_decl:
| VAR LPAREN RPAREN SEMICOLON
    { Empty }
| VAR LPAREN vds=var_decls RPAREN SEMICOLON
    { vds }
| VAR vdl=var_decl_line
    { vdl }
| var_id=IDEN COLONEQ e=expr SEMICOLON
    { Var_decl([var_id], Some([e]), None) }
| IDEN COLONEQ error
    { Error.print_error $startpos "error at variable declaration" }

var_decls:
| vds=var_decls vdl=var_decl_line
    { ignore(vds); vdl }
| vdl=var_decl_line
    { vdl }

var_decl_line:
| var_ids=identifiers typ_id=option(IDEN) ASSIGNMENT exprs=expressions SEMICOLON
    { ignore(check_balance (var_ids, exprs) $startpos);
      Var_decl(var_ids, Some(exprs), typ_id) }
| var_ids=identifiers typ_id=IDEN SEMICOLON
    { Var_decl(var_ids, None, Some(typ_id)) }
| var_ids=identifiers LBRACKET RBRACKET typ_id=IDEN SEMICOLON
    { Slice_decl(var_ids, typ_id) }
| var_ids=identifiers LBRACKET i=INT RBRACKET typ_id=IDEN SEMICOLON
    { Array_decl(var_ids, i, typ_id) }

type_decl:
| TYPE LPAREN RPAREN SEMICOLON
    { Empty }
| TYPE LPAREN tds=type_decls RPAREN SEMICOLON
    { tds }
| TYPE td=type_decl
    { td }
| TYPE error
    { Error.print_error $startpos "error at type declaration" }

type_decls:
| tds=type_decls tdl=type_decl_line
    { ignore(tds); tdl }
| tdl=type_decl_line
    { tdl }

type_decl_line:
| var_id=IDEN typ_id=IDEN SEMICOLON
    { Type_decl(var_id, typ_id) }
| var_id=IDEN STRUCT LBRACE var_ids=identifiers typ_id=IDEN SEMICOLON
  RBRACE SEMICOLON
    { Struct_decl(var_id, var_ids, typ_id) }

func_decl:
| FUNC fun_id=IDEN LPAREN params=parameters RPAREN
  LBRACE stmts=stmt* RETURN SEMICOLON RBRACE SEMICOLON
    { Func_decl(fun_id, params, stmts, None, None) }
| FUNC fun_id=IDEN LPAREN params=parameters RPAREN typ_id=IDEN
  LBRACE stmts=stmt* RETURN var_id=IDEN SEMICOLON RBRACE SEMICOLON
    { Func_decl(fun_id, params, stmts, Some(var_id), Some(typ_id)) }

identifiers:
| ids=separated_nonempty_list(COMMA, IDEN)
    { ids }

expressions:
| exprs=separated_nonempty_list(COMMA, expr)
    { exprs }

parameters:
| params=separated_list(COMMA, param_expr)
    { params }

param_expr:
| var_ids=identifiers typ_id=IDEN
    { (var_ids, typ_id) }

stmts_block:
| LBRACE stmts=stmt* RBRACE
    { stmts }

if_stmt:
| IF e=expr b=stmts_block
    { If_stmt(e, b, None) }
| IF e=expr b1=stmts_block ELSE b2=stmts_block
    { If_stmt(e, b1, Some(b2)) }
| IF e=expr b1=stmts_block ELSE b2=if_stmt
    { If_stmt(e, b1, Some([b2])) }

(*
switch_stmt:
| 
*)

for_stmt:
| FOR b=stmts_block
    { For_stmt(None, b) }
| FOR e=expr b=stmts_block
    { For_stmt(Some(e, None), b) }
| FOR i=assignment SEMICOLON e=expr SEMICOLON u=assignment
      b=stmts_block
    { For_stmt(Some(e, Some((i,u))), b)}

var_stmt:
| VAR LPAREN RPAREN SEMICOLON
    { Empty }
| VAR LPAREN vss=var_stmts RPAREN SEMICOLON
    { vss }
| VAR vsl=var_stmt_line
    { vsl }
| var_id=IDEN COLONEQ e=expr SEMICOLON
    { Var_stmt([var_id], Some([e]), None) }
| IDEN COLONEQ error
    { Error.print_error $startpos "error at variable declaration" }

var_stmts:
| vss=var_stmts vsl=var_stmt_line
    { ignore(vss); vsl }
| vsl=var_stmt_line
    { vsl }

var_stmt_line:
| var_ids=identifiers typ_id=option(IDEN) ASSIGNMENT exprs=expressions SEMICOLON
    { ignore(check_balance (var_ids, exprs) $startpos);
      Var_stmt(var_ids, Some(exprs), typ_id) }
| var_ids=identifiers typ_id=IDEN SEMICOLON
    { Var_stmt(var_ids, None, Some(typ_id)) }
| var_ids=identifiers LBRACKET RBRACKET typ_id=IDEN SEMICOLON
    { Slice_stmt(var_ids, typ_id) }
| var_ids=identifiers LBRACKET i=INT RBRACKET typ_id=IDEN SEMICOLON
    { Array_stmt(var_ids, i, typ_id) }

type_stmt:
| TYPE LPAREN RPAREN SEMICOLON
    { Empty }
| TYPE LPAREN tss=type_stmts RPAREN SEMICOLON
    { tss }
| TYPE tsl=type_stmt_line
    { tsl }
| TYPE error
    { Error.print_error $startpos "error at type declaration" }

type_stmts:
| tss=type_stmts tsl=type_stmt_line
    { ignore(tss); tsl }
| tsl=type_stmt_line
    { tsl }

type_stmt_line:
| var_id=IDEN typ_id=IDEN SEMICOLON
    { Type_stmt(var_id, typ_id) }
| var_id=IDEN STRUCT LBRACE var_ids=identifiers typ_id=IDEN SEMICOLON
  RBRACE SEMICOLON
    { Struct_stmt(var_id, var_ids, typ_id) }

stmt:
| a=assignment SEMICOLON
    { Assign(a) }
| is=if_stmt SEMICOLON
    { is }
(*| ss=switch_stmt SEMICOLON
    { ss }*)
| fs=for_stmt SEMICOLON
    { fs }
| vs=var_stmt
    { vs }
| ts=type_stmt
    { ts }
| error
    { Error.print_error $startpos "error at statement" }

%inline e_binop:
| PLUS  { Plus }
| MINUS { Minus }
| TIMES { Times }
| DIV   { Div }

%inline e_prefix_op:
| PLUS  { Pos }
| MINUS { Neg }

expr:
| n=INT
    { ILit(n) }
| f=FLOAT64
    { FLit(f) }
| b=BOOL
    { BLit(b) }
| c=RUNE
    { RLit(c) }
| s=STRING
    { SLit(s) }
| id=IDEN
    { Iden(id) }
| LPAREN e=expr RPAREN
    { e }
| e1=expr binop=e_binop e2=expr
    { Bexp(binop, e1, e2) }
| unop=e_prefix_op e=expr
    { Uexp(unop, e) }

%inline a_binop:
| TIMESEQ { Times }
| DIVEQ   { Div }
| PLUSEQ  { Plus }
| MINUSEQ { Minus }

%inline a_postfix:
| INC { Plus }
| DEC { Minus }

assignment:
| id=IDEN binop=a_binop e=expr
    { [(id, Bexp(binop, Iden(id), e))] }
| id=IDEN postfix=a_postfix
    { [(id, Bexp(postfix, Iden(id), ILit(1)))] }

(*
multiple_assign:
| ids=identifiers ASSIGNMENT exprs=expressions
    { ignore(check_balance (ids, exprs) $startpos);
      Assign(ids, exprs) }

inferred_assign:
| var_id=IDEN COLONEQ e=expr SEMICOLON
    { Inferred_assign([var_id], Some([e]), None) }
| IDEN COLONEQ error
    { Error.print_error $startpos "error at variable declaration" }
*)

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