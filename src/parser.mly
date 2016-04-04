%{
  open Ast
  
  let check_balance (vars, exprs) pos =
    if List.length vars <> List.length exprs
    then Error.print_error pos "unbalanced variables and expressions"
%}

%start <Untyped.ast> program

%%

program:
| pkg=package decls=decl* EOF
    { Prog(pkg, decls) }
(*
| decls=decl* EOF
    { Error.print_error
        $startpos 
        "A package declaration is needed at the beginning of the program" }
*)

(* Is this case useful? *)
| error
    { Error.print_error $startpos "syntax error" }


id:
| i=IDEN
  { (i, $startpos) }


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
    { (Var_decl(vd), $startpos) }
| td=type_decl SEMICOLON
    { (Type_decl(td), $startpos) }
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
| LPAREN t=typ RPAREN
  { t }


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
    { try
        (List.map2 (fun i e -> (i, Some(e), t)) var_ids exprs)
      with
        | Invalid_argument(_) -> Error.print_error $startpos "There must be as many expression as identifier in declaration" }
| var_ids=identifiers t=typ
    { List.map (fun i -> (i, None, Some(t))) var_ids }



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
      | Some(t) -> (Func_decl(fun_id, params, t, b), $startpos)
      | None    -> (Func_decl(fun_id, params, TVoid, b), $startpos)
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

stmt_no_decl:
| b=stmts_block SEMICOLON
    { (Block(b), $startpos) }
| a=assignment SEMICOLON
    { a }
| sd=short_decl SEMICOLON
    { sd }
| ss=switch_stmt SEMICOLON
    { (Block([ss]), $startpos) }
| fs=for_stmt SEMICOLON
    { (Block([fs]), $startpos) }
| ps=print_stmt SEMICOLON
    { ps }
| is=if_stmt SEMICOLON
    { (Block([is]), $startpos) }
| RETURN e=expr? SEMICOLON
    { (Return(e), $startpos) }
| BREAK SEMICOLON
    { (Break, $startpos) }
| CONTINUE SEMICOLON
    { (Continue, $startpos) }
| e=expr SEMICOLON
    { (Expr_stmt(e), $startpos) }
| error
    { Error.print_error $startpos "error at statement" }


stmt:
| s=stmt_no_decl
    { s }
| vs=var_decl SEMICOLON
    { (Var_stmt(vs), $startpos) }
| ts=type_decl SEMICOLON
    { (Type_stmt(ts), $startpos) }
| SEMICOLON 
    { (Empty_stmt, $startpos) }

(*
expr_stmt:
| e=expr
    { Fn_call(e, el, {Untyped.Info.pos=$startpos}) }
*)

stmts_block:
| LBRACE stmts=stmts RBRACE
    { stmts }

for_init_stmt:
| a=assignment
    { a }
| sd=short_decl
    { sd }
| e=expr
    { (Expr_stmt(e), $startpos) }

init_stmt:
| SEMICOLON
    { (Empty_stmt, $startpos) }
| a=assignment SEMICOLON
    { a }
| sd=short_decl SEMICOLON
    { sd }
| e=expr SEMICOLON
    { (Expr_stmt(e), $startpos) }

post_stmt:
| a=assignment
    { a }
| e=expr
    { (Expr_stmt(e), $startpos) }

if_stmt:
| IF is=ioption(init_stmt) e=expr b=stmts_block
    { (If_stmt(is, e, b, None), $startpos) }
| IF is=ioption(init_stmt) e=expr b1=stmts_block ELSE b2=stmts_block
    { (If_stmt(is, e, b1, Some(b2)), $startpos) }
| IF is=ioption(init_stmt) e=expr b1=stmts_block ELSE b2=if_stmt
    { (If_stmt(is, e, b1, Some([b2])), $startpos) }


switch_stmt:
| SWITCH is=ioption(init_stmt) e=expr? LBRACE sc=switch_clause* RBRACE
    { (Switch_stmt(is, e, sc), $startpos) }

switch_clause:
| sc=switch_case COLON stmts=stmts
    { (Switch_clause(sc, stmts), $startpos) }

switch_case:
| CASE exprs=expressions
    { Some(exprs) }
| DEFAULT
    { None }

for_stmt:
| FOR e=expr? b=stmts_block
    { (For_stmt(None, e, None, b), $startpos) }
| FOR is=for_init_stmt? SEMICOLON e=expr? SEMICOLON ps=post_stmt?
      b=stmts_block
    { (For_stmt(is, e, ps, b), $startpos) }

print_stmt:
| PRINT LPAREN exprs=separated_list(COMMA, expr) RPAREN
    { (Print(exprs), $startpos) }
| PRINTLN LPAREN exprs=separated_list(COMMA, expr) RPAREN
    { (Println(exprs), $startpos) }

(* expression... *)
short_decl:
| ids=expressions COLONEQ exprs=expressions
    {
      let defs =
      try
        List.map2 (fun id exp -> match id with
                    | (Iden(x),_) -> (x, exp)
                    | _ -> Error.print_error $startpos "ill-formed short declaration: identifiers expected" )
                  ids
                  exprs
      with
        | Invalid_argument(_) -> Error.print_error $startpos "There must be as many expression as identifier in declaration"
      in (SDecl_stmt(defs), $startpos)
    }
| expressions COLONEQ error
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
| es=expressions ASSIGNMENT exprs=expressions
    { ignore(check_balance (es, exprs) $startpos); 
      (Assign(es, exprs), $startpos) }

binop_assign:
| e1=expr binop=a_binop e2=expr
    { (Assign([e1], [(Bexp(binop, e1, e2), $startpos)]), $startpos) }

postfix_assign:
| e=expr postfix=a_postfix
    { (Assign([e], [(Bexp(postfix, e, (ILit(1), $startpos) ), $startpos)]), $startpos) }

(*
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
*)
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
| id=id
    { (Iden(id), $startpos) }
| e=expr LBRACKET i=expr RBRACKET
    { (AValue(e, i), $startpos) }
| e=expr DOT structs_id=id
    { (SValue(e, structs_id), $startpos) }
| LPAREN e=expr RPAREN
    { e (*(Parens(e), $startpos)*) }
| n=INT
    { (ILit(n), $startpos) }
| f=FLOAT64
    { (FLit(f), $startpos) }
| c=RUNE
    { (RLit(c), $startpos) }
| s=STRING
    { (SLit(s), $startpos) }
| e1=expr binop=e_binop e2=expr
    { (Bexp(binop, e1, e2), $startpos) }
| unop=e_prefix_op e=expr %prec UOP
    { (Uexp(unop, e), $startpos) }
| e=expr LPAREN el=separated_list(COMMA, expr) RPAREN
    { (Fn_call(e, el), $startpos) }
| APPEND LPAREN id=id COMMA e=expr RPAREN
    { (Append(id, e), $startpos) }




(* Production rules for error handling *)
