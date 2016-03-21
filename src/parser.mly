%{
  open Ast
  
  let check_balance (vars, exprs) pos =
    if List.length vars <> List.length exprs
    then Error.print_error pos "unbalanced variables and expressions"
%}

%start <Ast.Untyped.ast> program

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

id:
| i=IDEN
  { (i, {Untyped.Info.pos=$startpos}) }


(*
 * Useful rules
 *)
identifiers:
| ids=separated_nonempty_list(COMMA, id)
    { ids }

expressions:
| exprs=separated_nonempty_list(COMMA, expr)
    { exprs }



(*
 * Top level
 *)
package:
| PACKAGE i=id SEMICOLON
    { i }
(* Error handling *)
| PACKAGE error
    { Error.print_error $startpos "package identifier" }
| id SEMICOLON
    { Error.print_error $startpos "Missing `package` keyword"}

decl:
| vd=var_decl SEMICOLON
    { Var_decl(vd,{Untyped.Info.pos=$startpos}) }
| td=type_decl SEMICOLON
    { Type_decl(td,{Untyped.Info.pos=$startpos}) }
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
| ids=separated_nonempty_list(COMMA, id) t=typ SEMICOLON
  { List.map (function (id) -> (id, t)) ids }

struct_inner_decls_list:
| sid=struct_inner_decls
  { sid }
| sidl=struct_inner_decls_list sid=struct_inner_decls
  {List.append sidl sid}

typ:
| t=id
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
| i=id t=typ SEMICOLON
    { (i,t) }

type_decl:
| TYPE LPAREN tidl=id_type_pair* RPAREN
    { tidl }
| TYPE id=id t=typ
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
| var_ids=separated_nonempty_list(COMMA, id) t=typ
    { List.map (function (id) -> (id,t)) var_ids }

func_decl:
| FUNC fun_id=id LPAREN params=parameters RPAREN t=typ?
  b=stmts_block
    {
      match t with
      | Some(t) -> Func_decl(fun_id, params, t, b, {Untyped.Info.pos=$startpos})
      | None    -> Func_decl(fun_id, params, Void, b, {Untyped.Info.pos=$startpos})
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
    { Var_stmt(vs,{Untyped.Info.pos=$startpos}) }
| ts=type_decl SEMICOLON
    { Type_stmt(ts,{Untyped.Info.pos=$startpos}) }
| SEMICOLON 
    { Empty_stmt({Untyped.Info.pos=$startpos}) }
(*
| es=expr_stmt SEMICOLON
    { Expr_stmt(es) }
*)
(*
| SEMICOLON { Empty_stmt }
| error
    { Error.print_error $startpos "error at statement" }
*)


expr_stmt:
| lvl=callable_lvalue LPAREN el=separated_list(COMMA, expr) RPAREN
    { Fn_call(lvl, el, {Untyped.Info.pos=$startpos}) }

stmts_block:
| LBRACE stmts=stmts RBRACE
    { stmts }

for_init_stmt:
| a=assignment
    { a }
| sd=short_decl
    { sd }
| es=expr_stmt
    { Expr_stmt(es,{Untyped.Info.pos=$startpos}) }

init_stmt:
| SEMICOLON
    { Empty_stmt({Untyped.Info.pos=$startpos}) }
| a=assignment SEMICOLON
    { a }
| sd=short_decl SEMICOLON
    { sd }
| es=expr_stmt SEMICOLON
    { Expr_stmt(es,{Untyped.Info.pos=$startpos}) }

post_stmt:
| a=assignment
    { a }
| es=expr_stmt
    { Expr_stmt(es,{Untyped.Info.pos=$startpos}) }

if_stmt:
| IF is=ioption(init_stmt) e=expr b=stmts_block
    { If_stmt(is, e, b, None,{Untyped.Info.pos=$startpos}) }
| IF is=ioption(init_stmt) e=expr b1=stmts_block ELSE b2=stmts_block
    { If_stmt(is, e, b1, Some(b2),{Untyped.Info.pos=$startpos}) }
| IF is=ioption(init_stmt) e=expr b1=stmts_block ELSE b2=if_stmt
    { If_stmt(is, e, b1, Some([b2]),{Untyped.Info.pos=$startpos}) }


switch_stmt:
| SWITCH is=ioption(init_stmt) e=expr? LBRACE sc=switch_clause* RBRACE
    { Switch_stmt(is, e, sc, {Untyped.Info.pos=$startpos}) }

switch_clause:
| sc=switch_case COLON stmts=stmts
    { Switch_clause(sc, stmts, {Untyped.Info.pos=$startpos}) }

switch_case:
| CASE exprs=expressions
    { Some(exprs) }
| DEFAULT
    { None }

for_stmt:
| FOR e=expr? b=stmts_block
    { For_stmt(None, e, None, b,{Untyped.Info.pos=$startpos}) }
| FOR is=for_init_stmt? SEMICOLON e=expr? SEMICOLON ps=post_stmt?
      b=stmts_block
    { For_stmt(is, e, ps, b,{Untyped.Info.pos=$startpos}) }

print_stmt:
| PRINT LPAREN exprs=separated_list(COMMA, expr) RPAREN
    { Print(exprs,{Untyped.Info.pos=$startpos}) }
| PRINTLN LPAREN exprs=separated_list(COMMA, expr) RPAREN
    { Println(exprs,{Untyped.Info.pos=$startpos}) }

short_decl:
| lvls=lvalues COLONEQ exprs=expressions
    { ignore(check_balance (lvls, exprs) $startpos);
      let var_ids =
        List.map (function
                  | LIden(x,_) -> x
                  | _ -> Error.print_error $startpos "ill-formed short declaration: identifiers expected" )
                 lvls
      in SDecl_stmt(var_ids, Some(exprs), {Untyped.Info.pos=$startpos}) }
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
      Assign(List.rev lvs, exprs, {Untyped.Info.pos=$startpos}) }

binop_assign:
| lv=lvalue binop=a_binop e=expr
    { Assign([lv], [Bexp(binop, LValue(lv), e, {Untyped.Info.pos=$startpos})],{Untyped.Info.pos=$startpos}) }

postfix_assign:
| lv=lvalue postfix=a_postfix
    { Assign([lv], [Bexp(postfix, LValue(lv), ILit(1, {Untyped.Info.pos=$startpos}), {Untyped.Info.pos=$startpos})], {Untyped.Info.pos=$startpos}) }

callable_lvalue:
| id=id
    { LIden(id ,{Untyped.Info.pos=$startpos}) }
| lvl=lvalue DOT structs_id=id
    { LSValue(lvl, structs_id, {Untyped.Info.pos=$startpos}) }

lvalues:
| lvls=lvalues COMMA lvl=lvalue
    { lvl :: lvls }
| lvl=lvalue
    { [lvl] }

lvalue:
| id=id
    { LIden(id, {Untyped.Info.pos=$startpos}) }
| lvl=lvalue LBRACKET e=expr RBRACKET
    { LAValue(lvl, e, {Untyped.Info.pos=$startpos}) }
| lvl=lvalue DOT structs_id=id
    { LSValue(lvl, structs_id, {Untyped.Info.pos=$startpos}) }
(*
| LPAREN l=lvalue RPAREN
    { l }
*)


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
| l=lvalue
    { LValue(l) }
(*| id=IDEN
    { Iden(id,$startpos) }
| e=expr LBRACKET i=expr RBRACKET
    { AValue(e, i,$startpos) }
| e=expr DOT structs_id=IDEN
    { SValue(e, structs_id, $startpos) }*)
| LPAREN e=expr RPAREN
    { e }
| n=INT
    { ILit(n,{Untyped.Info.pos=$startpos}) }
| f=FLOAT64
    { FLit(f,{Untyped.Info.pos=$startpos}) }
| b=BOOL
    { BLit(b,{Untyped.Info.pos=$startpos}) }
| c=RUNE
    { RLit(c,{Untyped.Info.pos=$startpos}) }
| s=STRING
    { SLit(s,{Untyped.Info.pos=$startpos}) }
| e1=expr binop=e_binop e2=expr
    { Bexp(binop, e1, e2, {Untyped.Info.pos=$startpos}) }
| unop=e_prefix_op e=expr %prec UOP
    { Uexp(unop, e, {Untyped.Info.pos=$startpos}) }
| lvl=callable_lvalue LPAREN el=separated_list(COMMA, expr) RPAREN
    { Fn_call(lvl, el, {Untyped.Info.pos=$startpos}) }
| APPEND LPAREN id=id COMMA e=expr RPAREN
    { Append(id, e, {Untyped.Info.pos=$startpos}) }





(* Production rules for error handling *)
stmt_no_decl:
| b=stmts_block SEMICOLON
    { Block(b, {Untyped.Info.pos=$startpos}) }
| a=assignment SEMICOLON
    { a }
| sd=short_decl SEMICOLON
    { sd }
| ss=switch_stmt SEMICOLON
    { Block([ss], {Untyped.Info.pos=$startpos}) }
| fs=for_stmt SEMICOLON
    { Block([fs], {Untyped.Info.pos=$startpos}) }
| ps=print_stmt SEMICOLON
    { ps }
| is=if_stmt SEMICOLON
    { Block([is], {Untyped.Info.pos=$startpos}) }
| RETURN e=expr? SEMICOLON
    { Return(e, {Untyped.Info.pos=$startpos}) }
| BREAK SEMICOLON
    { Break({Untyped.Info.pos=$startpos}) }
| CONTINUE SEMICOLON
    { Continue({Untyped.Info.pos=$startpos}) }
| es=expr_stmt SEMICOLON
    { Expr_stmt(es, {Untyped.Info.pos=$startpos}) }
| error
    { Error.print_error $startpos "error at statement" }
