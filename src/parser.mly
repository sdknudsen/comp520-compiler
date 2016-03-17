%{
  open Ast
%}

%start <Ast.ast> program

%%

program:
| pkg=package decls=decl* EOF
    { Prog(pkg, decls) }
| decls=decl* EOF
    { Error.print_error
        $startpos 
        "A package declaration is needed at the beginning of the program" }
(* Is this case useful? *)
(*
  | error
    { Error.print_error $startpos "syntax error" }
*)


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
(* Error handling *)
| PACKAGE error
    { Error.print_error $startpos "package identifier" }
| IDEN SEMICOLON
    { Error.print_error $startpos "Missing `package` keyword"}

decl:
| vd=var_decl SEMICOLON
    { Var_decl(vd) }
| td=type_decl SEMICOLON
    { Type_decl(td) }
| fd=func_decl SEMICOLON
    { fd }
| SEMICOLON
    { Error.print_error
        $startpos 
        "Empty declaration" } 
| stmt_no_decl
    { Error.print_error
        $startpos 
        "Statements can't be at the toplevel" }



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
  { TSimp(t) }
| STRUCT LBRACE RBRACE
  { TStruct([]) }
| STRUCT LBRACE sidl=struct_inner_decls_list RBRACE
  { TStruct(sidl) }
| LBRACKET n=INT RBRACKET t=typ
  { TArray(t, n) }
| LBRACKET RBRACKET t=typ
  { TSlice(t) }

id_type_pair:
| i=IDEN t=typ SEMICOLON
    { (i,t) }

type_decl:
| TYPE LPAREN tidl=id_type_pair* RPAREN
    { tidl }
| TYPE id=IDEN t=typ
    { [(id,t)] }
(* Error handling *)
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
stmts:
| sl=stmt*
    { sl }



stmt:
| s=stmt_no_decl
    { s }
(*
| a=assignment SEMICOLON
    { a }
| sd=short_decl SEMICOLON
    { sd }
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
*)
| vs=var_decl SEMICOLON
    { Var_stmt(vs) }
| ts=type_decl SEMICOLON
    { Type_stmt(ts) }
| SEMICOLON 
    { Empty_stmt }
(*
| es=expr_stmt SEMICOLON
    { Expr_stmt(es) }
| SEMICOLON { Empty_stmt }
| error
    { Error.print_error $startpos "error at statement" }
*)

expr_stmt:
| lvl=callable_lvalue LPAREN el=separated_list(COMMA, expr) RPAREN
    { Fn_call(lvl, el) }

stmts_block:
| LBRACE stmts=stmts RBRACE
    { stmts }

for_init_stmt:
| a=assignment
    { a }
| sd=short_decl
    { sd }
| es=expr_stmt
    { Expr_stmt(es) }

init_stmt:
| SEMICOLON
    { Empty_stmt}
| a=assignment SEMICOLON
    { a }
| sd=short_decl SEMICOLON
    { sd }
| es=expr_stmt SEMICOLON
    { Expr_stmt(es) }

post_stmt:
| a=assignment
    { a }
| es=expr_stmt
    { Expr_stmt(es) }

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
| sc=switch_case COLON stmts=stmts
    { Switch_clause(sc, stmts) }

switch_case:
| CASE exprs=expressions
    { Some(exprs) }
| DEFAULT
    { None }

for_stmt:
| FOR e=expr? b=stmts_block
    { For_stmt(None, e, None, b) }
| FOR is=for_init_stmt? SEMICOLON e=expr? SEMICOLON ps=post_stmt?
      b=stmts_block
    { For_stmt(is, e, ps, b) }

print_stmt:
| PRINT LPAREN exprs=separated_list(COMMA, expr) RPAREN
    { Print(exprs) }
| PRINTLN LPAREN exprs=separated_list(COMMA, expr) RPAREN
    { Println(exprs) }

short_decl:
| lvls=lvalues COLONEQ exprs=expressions
    { ignore(check_balance (lvls, exprs) $startpos);
      let var_ids =
        List.map (function
                  | Iden(x) -> x
                  | _ -> Error.print_error $startpos "ill-formed short declaration: identifiers expected" )
                 lvls
      in SDecl_stmt(var_ids, Some(exprs)) }
| lvalues COLONEQ error
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





(* Production rules for error handling *)
stmt_no_decl:
| b=stmts_block SEMICOLON
    { Block(b) }
| a=assignment SEMICOLON
    { a }
| sd=short_decl SEMICOLON
    { sd }
| ss=switch_stmt SEMICOLON
    { Block([ss]) }
| fs=for_stmt SEMICOLON
    { Block([fs]) }
| ps=print_stmt SEMICOLON
    { ps }
| is=if_stmt SEMICOLON
    { Block([is]) }
| RETURN e=expr? SEMICOLON
    { Return(e) }
| BREAK SEMICOLON
    { Break }
| CONTINUE SEMICOLON
    { Continue }
| es=expr_stmt SEMICOLON
    { Expr_stmt(es) }
| error
    { Error.print_error $startpos "error at statement" }
