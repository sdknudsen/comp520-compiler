module Ctx = Map.Make(String)
(* make a new module that's a list of maps? *)

type id = string
(* can a variable have the same name as a type? *)
type var_id = id
type typ_id = id
type fun_id = id
type pkg_id = id

type binop = Boolor | Booland |
             Equals | Notequals | Lt | Lteq | Gt | Gteq |
             Plus | Minus | Bitor | Bitxor |
             Times | Div | Modulo | Lshift | Rshift | Bitand | Bitnand
type unop = Positive | Negative | Boolnot | Bitnot

(* Type declarations *)
type typ =
  | Simple_type of typ_id
  | Struct_type of (typ_id * typ) list
  | Array_type  of typ
  | Slice_type  of typ
  | Void


(*type ('e, 'l) lvalueF =*)
(* EXpressions *)
type ('e, 'l) exprF =
  | Lvalue of 'l
  (* | TIden of tp_id *)
  | ILit of int
  | FLit of float
  | BLit of bool
  | RLit of char
  | SLit of string
  | Uexp of unop * 'e
  | Bexp of binop * 'e * 'e
  | Func of fun_id * 'e list
  | Append of 'l * 'e

(* Lvalues *)
type ('e, 'l) lvalueF = 
  | Iden of id
  | AValue of 'l * 'e
  | SValue of 'l * id

type expr  = (expr, lvalue) exprF
and lvalue = (expr, lvalue) lvalueF

(* reverse t_rec and t_expr so that t_stmt doesn't need a type field? *)
(*
type t_expr = t_rec exprF
and t_rec = { exp : t_expr; typ : id; }
*)
(*type t_expr = (t_expr * id) exprF*)



type ('l, 'e) assignment = 'l list * 'e list
(* | Assign of 'e assignment *)

(* Statements *)
type ('e, 'l, 's) stmtF =
  | Assign  of ('l, 'e) assignment
  | Print   of 'e list
  | Println of 'e list
  | If_stmt of 's option * 'e * 's list * 's list option
  (* | If_stmt of 'e * 's list * 's list option *)
  | Switch_stmt of 's option * 'e option * 's list
  (* | Switch_stmt of 'e option * 's list *)
  | Switch_clause of 'e list option * 's list
  | For_stmt    of 's option * 'e option * 's option * 's list
  | Var_stmt    of (id list * 'e list option * typ option) list
  | SDecl_stmt  of (id list * 'e list option)
  | Type_stmt   of (id * typ) list
  | Return      of 'e option
  | Break
  | Continue
  | Empty
and stmt = (expr, lvalue, stmt) stmtF
(*type t_stmt = (t_expr * id, t_stmt) stmtF*)

(* Top-level declarations *)
type ('e,'s) declF =
  | Var_decl  of (id list * 'e list option * typ option) list
  | Type_decl of (typ_id * typ) list
  | Func_decl of fun_id * (id * typ) list * typ * 's list
and decl = (expr, stmt) declF

(* type declaration = Dec of id * id *)
type package = Pkg of pkg_id

type ast = Prog of package * decl list
(* type t_ast = TProg of t_stmt list *)

(* 
let str_of_binop = function
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | DIV -> "/"
  | _ -> failwith "string not yet declared"

let str_of_unop = function
  | NEG -> "-"
  | _ -> failwith "string not yet declared"
 *)
(* get a typed declaration list from the reversed declarations in the parser *)
let rev_decls ds = 
  let rec get_ls_tup = function
    | [] -> ([], [], None)
    | [(var, expr, Some t)] -> ([var], [expr], Some t)
    | (var, expr, None)::tl -> let (vs, es, tp) = get_ls_tup tl in
                               (var::vs, expr::es, tp)
    | _ -> failwith "error" (* change the error *)
  in
  let rec zipDecls = function
    | x::xs, y::ys, tp -> (x,y,tp)::zipDecls(xs,ys,tp)
    | _ -> []
  in
  let (vs, es, t) = get_ls_tup ds in
  zipDecls(vs,(List.rev es),t)

              (* (id * expr * typ option) list *)

let check_balance (vars, exprs) pos =
  if List.length vars <> List.length exprs
  then Error.print_error pos "unbalanced variables and expressions"
