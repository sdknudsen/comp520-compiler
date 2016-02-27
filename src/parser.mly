%{
  open Ast
%}

%start <Ast.ast> program

%%

program:
| pkg=package decls=decl* EOF
    { Prog(pkg, decls) }
| error
    { Error.print_error $startpos "syntax error" }

(* so that the compiler doesn't complain about unused tokens *)
(* check that we don't use any of these !! *)
| INTERFACE 
| SELECT
| CHAN
| CONST
| DEFER
| ELLIPSIS
| FALLTHROUGH
| GO
| GOTO
| IMPORT
| LARROW
| MAP
| RANGE { Error.print_error $startpos "Use of reserved keyword" }


(*
 * Useful rules
 *)
identifiers:
| ids=separated_nonempty_list(COMMA, IDEN)
    { ids }

expressions:
| exprs=separated_nonempty_list(COMMA, expr)
    { exprs }


(*
 * Top level
 *)
package:
| PACKAGE pkg_id=IDEN SEMICOLON
    { pkg_id }
| PACKAGE error 
    { Error.print_error $startpos "package identifier" }

decl:
| vd=var_decl SEMICOLON
    { Var_decl(vd) }
| td=type_decl SEMICOLON
    { Type_decl(td) }
| fd=func_decl SEMICOLON
    { fd }



(*
 * Type declaration
 *)
struct_inner_decls:
| ids=separated_nonempty_list(COMMA, IDEN) t=typ SEMICOLON
  { List.map (function (id) -> (id, t)) ids }

struct_inner_decls_list:
| sid=struct_inner_decls
  { sid }
| sidl=struct_inner_decls_list sid=struct_inner_decls
  {List.append sidl sid}

typ:
| t=IDEN
  { Simple_type(t) }
| STRUCT LBRACE RBRACE
  { Struct_type([]) }
| STRUCT LBRACE sidl=struct_inner_decls_list RBRACE
  { Struct_type(sidl) }
| LBRACKET n=INT RBRACKET t=typ
  { Array_type(t, n) }
| LBRACKET RBRACKET t=typ
  { Slice_type(t) }

id_type_pair:
| i=IDEN t=typ SEMICOLON
    { (i,t) }

type_decl:
| TYPE LPAREN tidl=id_type_pair* RPAREN
    { tidl }
| TYPE id=IDEN t=typ
    { [(id,t)] }
| TYPE error
    { Error.print_error $startpos "error at type declaration" }



(*
 * Var declaration
 *)
var_decl:
| VAR LPAREN RPAREN
    { [] }
| VAR LPAREN vds=var_decls RPAREN
    { List.rev vds }
| VAR vdl=var_decl_line
    { [vdl] }

(* Short decls are not top level... *)
(*
| var_ids=lvalues COLONEQ exprs=expressions SEMICOLON
    { ignore(check_balance (var_ids, exprs) $startpos);
      Var_decl(var_ids, Some(exprs), None) }
| lvalues COLONEQ error
    { Error.print_error $startpos "error at variable declaration" }
*)

var_decls:
| vds=var_decls vdl=var_decl_line SEMICOLON
    { vdl :: vds }
| vdl=var_decl_line SEMICOLON
    { [vdl] }

var_decl_line:
| var_ids=identifiers t=typ? ASSIGNMENT exprs=expressions
    { ignore(check_balance (var_ids, exprs) $startpos);
      (var_ids, Some(exprs), t) }
| var_ids=identifiers t=typ
    { (var_ids, None, Some(t)) }


(*
 * Function declaration
 *)
param_expr:
| var_ids=separated_nonempty_list(COMMA, IDEN) t=typ
    { List.map (function (id) -> (id,t)) var_ids }

func_decl:
| FUNC fun_id=IDEN LPAREN params=parameters RPAREN t=typ?
  b=stmts_block
    {
      match t with
      | Some(t) -> Func_decl(fun_id, params, t, b)
      | None    -> Func_decl(fun_id, params, Void, b)
    }

parameters:
| params=separated_list(COMMA, param_expr)
    { List.concat params }



(*
 * Statements
 *)
stmt:
| a=assignment SEMICOLON
    { a }
(*
| sd=short_decl SEMICOLON
    { sd }
*)
| ss=switch_stmt SEMICOLON
    { ss }
| fs=for_stmt SEMICOLON
    { fs }
| ps=print_stmt SEMICOLON
    { ps }
| is=if_stmt SEMICOLON
    { is }
| RETURN e=expr? SEMICOLON
    { Return(e) }
| BREAK SEMICOLON
    { Break }
| CONTINUE SEMICOLON
    { Continue }
| vs=var_decl SEMICOLON
    { Var_stmt(vs) }
| ts=type_decl SEMICOLON
    { Type_stmt(ts) }
| es=expr_stmt SEMICOLON
    { Expr_stmt(es) }
| SEMICOLON
| error
    { Error.print_error $startpos "error at statement" }

expr_stmt:
| lvl=callable_lvalue LPAREN el=separated_list(COMMA, expr) RPAREN
    { Fn_call(lvl, el) }

stmts_block:
| LBRACE stmts=stmt* RBRACE
    { stmts }

init_stmt:
| a=assignment SEMICOLON
    { a }
(*
| sd=short_decl SEMICOLON
    { sd }
*)

if_stmt:
| IF is=ioption(init_stmt) e=expr b=stmts_block
    { If_stmt(is, e, b, None) }
| IF is=ioption(init_stmt) e=expr b1=stmts_block ELSE b2=stmts_block
    { If_stmt(is, e, b1, Some(b2)) }
| IF is=ioption(init_stmt) e=expr b1=stmts_block ELSE b2=if_stmt
    { If_stmt(is, e, b1, Some([b2])) }


switch_stmt:
| SWITCH is=ioption(init_stmt) e=expr? LBRACE sc=switch_clause* RBRACE
    { Switch_stmt(is, e, sc) }

switch_clause:
| sc=switch_case COLON stmts=stmt*
    { Switch_clause(sc, stmts) }

switch_case:
| CASE exprs=expressions
    { Some(exprs) }
| DEFAULT
    { None }

for_stmt:
| FOR e=expr? b=stmts_block
    { For_stmt(None, e, None, b) }
| FOR is=for_init_stmt? SEMICOLON e=expr? SEMICOLON a=assignment?
      b=stmts_block
    { For_stmt(is, e, a, b) }

for_init_stmt:
| a=assignment
    { a }
(*
| sd=short_decl
    { sd }
*)


print_stmt:
| PRINT LPAREN exprs=separated_list(COMMA, expr) RPAREN
    { Print(exprs) }
| PRINTLN LPAREN exprs=separated_list(COMMA, expr) RPAREN
    { Println(exprs) }

short_decl:
| var_ids=identifiers COLONEQ exprs=expressions
    { ignore(check_balance (var_ids, exprs) $startpos);
      SDecl_stmt(var_ids, Some(exprs)) }
| identifiers COLONEQ error
    { Error.print_error $startpos "error at variable declaration" }



%inline a_binop:
(* add_op *)
| PLUSEQ    { Plus }
| MINUSEQ   { Minus }
| BITOREQ   { Bitor }
| BITXOREQ  { Bitxor }
(* mul_op *)
| TIMESEQ   { Times }
| DIVEQ     { Div }
| PERCENTEQ { Modulo }
| LSHIFTEQ  { Lshift }
| RSHIFTEQ  { Rshift }
| AMPEQ     { Bitand }
| BITNANDEQ { Bitnand }

%inline a_postfix:
| INC       { Plus }
| DEC       { Minus }


assignment:
| sa=simple_assign
    { sa }
| ba=binop_assign
    { ba }
| pa=postfix_assign
    { pa }

simple_assign:
| lvs=lvalues ASSIGNMENT exprs=expressions
    { ignore(check_balance (lvs, exprs) $startpos); 
      Assign(List.rev lvs, exprs) }
binop_assign:
| lv=lvalue binop=a_binop e=expr
    { Assign([lv], [Bexp(binop, Lvalue(lv), e)]) }

postfix_assign:
| lv=lvalue postfix=a_postfix
    { Assign([lv], [Bexp(postfix, Lvalue(lv), ILit(1))]) }

callable_lvalue:
| id=IDEN
    { Iden(id) }
| lvl=lvalue DOT structs_id=IDEN
    { SValue(lvl, structs_id) }

lvalues:
| lvls=lvalues COMMA lvl=lvalue
    { lvl :: lvls }
| lvl=lvalue
    { [lvl] }

lvalue:
| id=IDEN
    { Iden(id) }
| lvl=lvalue LBRACKET e=expr RBRACKET
    { AValue(lvl, e) }
| lvl=lvalue DOT structs_id=IDEN
    { SValue(lvl, structs_id) }



(*
 * Expressions
 *)
%inline e_binop:
(* binary_op *)
| BOOL_OR    { Boolor }
| BOOL_AND   { Booland }
(* rel_op *)
| EQUALS     { Equals }
| NOTEQUALS  { Notequals }
| LCHEVRON   { Lt }
| LTEQ       { Lteq }
| RCHEVRON   { Gt }
| GTEQ       { Gteq }
(* add_op *)
| PLUS       { Plus }
| MINUS      { Minus }
| BITOR      { Bitor }
| CIRCUMFLEX { Bitxor }
| BITAND     { Bitand }
| BITNAND    { Bitnand }
(* mul_op *)
| TIMES      { Times }
| DIV        { Div }
| PERCENT    { Modulo }
| LSHIFT     { Lshift }
| RSHIFT     { Rshift }

%inline e_prefix_op:
| PLUS       { Positive }
| MINUS      { Negative }
| BANG       { Boolnot }
| CIRCUMFLEX { Bitnot }

expr:
| LPAREN e=expr RPAREN
    { e }
| lvl=lvalue
    { Lvalue(lvl) }
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
| e1=expr binop=e_binop e2=expr
    { Bexp(binop, e1, e2) }
| unop=e_prefix_op e=expr %prec UOP
    { Uexp(unop, e) }
| lvl=callable_lvalue LPAREN el=separated_list(COMMA, expr) RPAREN
    { Fn_call(lvl, el) }
| APPEND LPAREN id=IDEN COMMA e=expr RPAREN
    { Append(id, e) }

