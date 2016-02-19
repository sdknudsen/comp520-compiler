module Ctx = Map.Make(String)
(* make a new module that's a list of maps? *)

type id = string
(* can a variable have the same name as a type? *)
type var_id = id
type typ_id = id
type fun_id = id
type pkg_id = id

type binop = Plus | Minus | Times | Div
type unop = Neg | Pos

(* type info =  *)

type 'a exprF =
  | ILit of int
  | FLit of float
  | BLit of bool
  | RLit of char
  | SLit of string
  | Iden of id
  (* | TIden of tp_id *)
  | Bexp of binop * 'a * 'a
  | Uexp of unop * 'a
and expr = expr exprF

(* reverse t_rec and t_expr so that t_stmt doesn't need a type field? *)
type t_expr = t_rec exprF
and t_rec = { exp : t_expr; typ : id; }
(*type t_expr = (t_expr * id) exprF*)


type 'e assignment = (id * 'e) list

type ('e,'s) stmtF =
  | Assign of 'e assignment
  | Print of 'e
  | If_stmt of 'e * 's list * 's list option 
  | For_stmt of ('e * ('e assignment * 'e assignment) option) option
                * 's list
  | Var_stmt of var_id list * 'e list option * typ_id option
  | Slice_stmt of var_id list * typ_id
  | Array_stmt of var_id list * int * typ_id
  | Type_stmt of var_id * typ_id
  | Struct_stmt of var_id * var_id list * typ_id
  | Empty
and stmt = (expr, stmt) stmtF
(*type t_stmt = (t_expr * id, t_stmt) stmtF*)

type ('e,'s) declF =
  | Var_decl of var_id list * 'e list option * typ_id option
  | Slice_decl of var_id list * typ_id
  | Array_decl of var_id list * int * typ_id
  | Type_decl of var_id * typ_id
  | Struct_decl of var_id * var_id list * typ_id
  | Func_decl of fun_id * (var_id list * typ_id) list * 's list
                 * var_id option * typ_id option
  | Empty
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