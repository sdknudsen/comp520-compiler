(* removed rectypes for better error messages *)

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
  | TSimp of typ_id
  | TStruct of (typ_id * typ) list
  | TArray  of typ * int
  | TSlice  of typ
  | Void

(* Typed *)
type t_expr = { exp : t_rec; typ : typ; }
and t_rec =
  | Lvalue of t_lvalue
  | ILit of int
  | FLit of float
  | BLit of bool
  | RLit of char
  | SLit of string
  | Uexp of unop * t_expr
  | Bexp of binop * t_expr * t_expr
  | Fn_call of t_lvalue * t_expr list
  | Append of id * t_expr

and t_lvalue =
  | Iden of id
  | AValue of t_lvalue * t_expr
  | SValue of t_lvalue * id
type t_assignment = t_lvalue list * t_expr list

type t_stmt =
  | Assign  of t_assignment
  | Print   of t_expr list
  | Println of t_expr list
  | If_stmt of t_stmt option * t_expr * t_stmt list * t_stmt list option
  | Switch_stmt of t_stmt option * t_expr option * t_stmt list
  | Switch_clause of t_expr list option * t_stmt list
  | For_stmt    of t_stmt option * t_expr option * t_stmt option * t_stmt list
  | Var_stmt    of (id list * t_expr list option * typ option) list
  | SDecl_stmt  of (id list * t_expr list option)
  | Type_stmt   of (id * typ) list
  | Expr_stmt   of t_expr
  | Return      of t_expr option
  | Block       of t_stmt list (* is it alright to add this in? *)
  | Break
  | Continue
  | Empty_stmt

type t_decl =
  | Var_decl  of (id list * t_expr list option * typ option) list
  | Type_decl of (typ_id * typ) list
  | Func_decl of fun_id * (id * typ) list * typ * t_stmt list

type t_ast = TProg of pkg_id * t_decl list


(* Untyped *)

(* Expressions *)
(* Lvalues *)
type expr =
  | Lvalue of lvalue
  | ILit of int
  | FLit of float
  | BLit of bool
  | RLit of char
  | SLit of string
  | Uexp of unop * expr
  | Bexp of binop * expr * expr
  | Fn_call of lvalue * expr list
  | Append of id * expr
and lvalue = 
  | Iden of id
  | AValue of lvalue * expr
  | SValue of lvalue * id

type assignment = lvalue list * expr list

(* Statements *)
type stmt =
  | Assign  of assignment
  | Print   of expr list
  | Println of expr list
  | If_stmt of stmt option * expr * stmt list * stmt list option
  | Switch_stmt of stmt option * expr option * stmt list
  | Switch_clause of expr list option * stmt list
  | For_stmt    of stmt option * expr option * stmt option * stmt list
  | Var_stmt    of (id list * expr list option * typ option) list
  | SDecl_stmt  of (id list * expr list option)
  | Type_stmt   of (id * typ) list
  | Expr_stmt   of expr
  | Return      of expr option
  | Block       of stmt list
  | Break
  | Continue
  | Empty_stmt

(* Top-level declarations *)
type decl =
  | Var_decl  of (id list * expr list option * typ option) list
  | Type_decl of (typ_id * typ) list
  | Func_decl of fun_id * (id * typ) list * typ * stmt list

type ast = Prog of pkg_id * decl list

let check_balance (vars, exprs) pos =
  if List.length vars <> List.length exprs
  then Error.print_error pos "unbalanced variables and expressions"
