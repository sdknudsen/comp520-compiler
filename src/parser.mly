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
| RANGE	{ Error.print_error $startpos "Use of reserved keyword" }

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
| var_ids=lvalues COLONEQ exprs=expressions SEMICOLON
    { ignore(check_balance (var_ids, exprs) $startpos);
      Var_decl(var_ids, Some(exprs), None) }
| lvalues COLONEQ error
    { Error.print_error $startpos "error at variable declaration" }

var_decls:
| vds=var_decls vdl=var_decl_line
    { ignore(vds); vdl }
| vdl=var_decl_line
    { vdl }

var_decl_line:
| var_ids=identifiers typ_id=type_name? ASSIGNMENT exprs=expressions SEMICOLON
    { ignore(check_balance (var_ids, exprs) $startpos);
      Var_decl(var_ids, Some(exprs), typ_id) }
| var_ids=identifiers typ_id=type_name SEMICOLON
    { Var_decl(var_ids, None, Some(typ_id)) }
| var_ids=identifiers LBRACKET RBRACKET typ_id=type_name SEMICOLON
    { Slice_decl(var_ids, typ_id) }
| var_ids=identifiers LBRACKET n=INT RBRACKET typ_id=type_name SEMICOLON 
    { Array_decl(var_ids, n, typ_id) }

type_decl:
| TYPE LPAREN RPAREN SEMICOLON
    { Empty }
| TYPE LPAREN tds=type_decls RPAREN SEMICOLON
    { tds }
| TYPE tdl=type_decl_line
    { tdl }
| TYPE error
    { Error.print_error $startpos "error at type declaration" }

type_decls:
| tds=type_decls tdl=type_decl_line
    { ignore(tds); tdl }
| tdl=type_decl_line
    { tdl }

type_decl_line:
| var_id=IDEN typ_id=type_name SEMICOLON
    { Type_decl(var_id, typ_id) }
| var_id=IDEN STRUCT LBRACE tss=type_structs RBRACE SEMICOLON
    { Struct_decl(var_id, tss) }

type_structs:
| tss=type_structs tsl=type_structs_line
    { ignore(tss); tsl }
| tsl=type_structs_line
    { tsl }

type_structs_line:
| var_ids=identifiers typ_id=type_name SEMICOLON
    { [(var_ids, typ_id)] }

func_decl:
| FUNC fun_id=IDEN LPAREN params=parameters RPAREN typ_id=type_name?
  b=stmts_block SEMICOLON
    { Func_decl(fun_id, params, typ_id, b) }

type_name:
| LPAREN typ_name=type_name RPAREN
    { typ_name }
| typ_id=IDEN
    { typ_id }

identifiers:
| ids=separated_nonempty_list(COMMA, identifier)
    { ids }

identifier:
| var_id=IDEN
    { Iden(var_id) }

expressions:
| exprs=separated_nonempty_list(COMMA, expr)
    { exprs }

parameters:
| params=separated_list(COMMA, param_expr)
    { params }

param_expr:
| var_ids=identifiers typ_id=type_name
    { (var_ids, typ_id) }

stmts_block:
| LBRACE stmts=stmt* RBRACE
    { stmts }

(*
init_stmt:
| a=assignment SEMICOLON
    { a }
| sd=short_decl SEMICOLON
    { sd }

if_stmt:
| IF is=init_stmt? e=expr b=stmts_block
    { If_stmt(is, e, b, None) }
| IF is=init_stmt? e=expr b1=stmts_block ELSE b2=stmts_block
    { If_stmt(is, e, b1, Some(b2)) }
| IF is=init_stmt? e=expr b1=stmts_block ELSE b2=if_stmt
    { If_stmt(is, e, b1, Some([b2])) }

switch_stmt:
| SWITCH is=init_stmt? e=expr? LBRACE sc=switch_clause* RBRACE
    { Switch_stmt(is, e, sc) }
*)

if_stmt:
| IF e=expr b=stmts_block
    { If_stmt(None, e, b, None) }
| IF e=expr b1=stmts_block ELSE b2=stmts_block
    { If_stmt(None, e, b1, Some(b2)) }
| IF e=expr b1=stmts_block ELSE b2=if_stmt
    { If_stmt(None, e, b1, Some([b2])) }

switch_stmt:
| SWITCH e=expr? LBRACE sc=switch_clause* RBRACE
    { Switch_stmt(None, e, sc) }

switch_clause:
| sc=switch_case COLON stmts=stmt*
    { Switch_clause(sc, stmts) }

switch_case:
| CASE exprs=expressions
    { Some(exprs) }
| DEFAULT
    { None }

for_stmt:
| FOR b=stmts_block
    { For_stmt(None, b) }
| FOR e=expr b=stmts_block
    { For_stmt(Some(e, None), b) }
| FOR sd=short_decl SEMICOLON e=expr SEMICOLON pa=postfix_assign
      b=stmts_block
    { For_stmt(Some(e, Some((sd,pa))), b)}

print_stmt:
| PRINT LPAREN exprs=expressions RPAREN
    { Print(exprs) }
| PRINTLN LPAREN exprs=expressions RPAREN
    { Println(exprs) }

short_decl:
| var_ids=lvalues COLONEQ exprs=expressions
    { ignore(check_balance (var_ids, exprs) $startpos);
      Var_stmt(var_ids, Some(exprs), None) }
| lvalues COLONEQ error
    { Error.print_error $startpos "error at variable declaration" }

var_stmt:
| VAR LPAREN RPAREN SEMICOLON
    { Empty }
| VAR LPAREN vss=var_stmts RPAREN SEMICOLON
    { vss }
| VAR vsl=var_stmt_line
    { vsl }

var_stmts:
| vss=var_stmts vsl=var_stmt_line
    { ignore(vss); vsl }
| vsl=var_stmt_line
    { vsl }

var_stmt_line:
| var_ids=identifiers typ_id=type_name? ASSIGNMENT exprs=expressions SEMICOLON
    { ignore(check_balance (var_ids, exprs) $startpos);
      Var_stmt(var_ids, Some(exprs), typ_id) }
| var_ids=identifiers typ_id=type_name SEMICOLON
    { Var_stmt(var_ids, None, Some(typ_id)) }
| var_ids=identifiers LBRACKET RBRACKET typ_id=type_name SEMICOLON
    { Slice_stmt(var_ids, typ_id) }
| var_ids=identifiers LBRACKET n=INT RBRACKET typ_id=type_name SEMICOLON
    { Array_stmt(var_ids, n, typ_id) }

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
| var_id=IDEN typ_id=type_name SEMICOLON
    { Type_stmt(var_id, typ_id) }
| var_id=IDEN STRUCT LBRACE tss=type_structs SEMICOLON
  RBRACE SEMICOLON
    { Struct_stmt(var_id, tss) }

stmt:
| a=assignment SEMICOLON
    { a }
| sd=short_decl SEMICOLON
    { sd }
| ps=print_stmt SEMICOLON
    { ps }
| is=if_stmt SEMICOLON
    { is }
| ss=switch_stmt SEMICOLON
    { ss }
| fs=for_stmt SEMICOLON
    { fs }
| RETURN e=expr? SEMICOLON
    { Return(e) }
| BREAK SEMICOLON
    { Break }
| CONTINUE SEMICOLON
    { Continue }
| vs=var_stmt
    { vs }
| ts=type_stmt
    { ts }
| error
    { Error.print_error $startpos "error at statement" }

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
| var_id=IDEN
    { Iden(var_id) }
| array_id=IDEN LBRACKET n=INT RBRACKET
    { AIden(array_id, n) }
| var_id=IDEN DOT structs_id=IDEN
    { SIden(var_id, structs_id) }
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
| fun_id=IDEN LPAREN ids=identifiers RPAREN
    { Func(fun_id, ids) }
| APPEND LPAREN var_id=IDEN COMMA e=expr RPAREN
    { Append(var_id, e) }

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
      Assign(lvs, exprs) }

binop_assign:
| lv=lvalue binop=a_binop e=expr
    { Assign([lv], [Bexp(binop, lv, e)]) }

postfix_assign:
| lv=lvalue postfix=a_postfix
    { Assign([lv], [Bexp(postfix, lv, ILit(1))]) }

lvalues:
| lvs=separated_nonempty_list(COMMA, lvalue)
    { lvs }

lvalue:
| id=identifier
    { id }
| array_id=IDEN LBRACKET n=INT RBRACKET
    { AIden(array_id, n) }
| var_id=IDEN DOT structs_id=IDEN
    { SIden(var_id, structs_id) }
