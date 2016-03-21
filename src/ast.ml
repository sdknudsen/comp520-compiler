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
  | TSimp   of typ_id
  | TStruct of (typ_id * typ) list
  | TArray  of typ * int
  | TSlice  of typ
  | Void

(* Typed *)
type t_rec =
  | Lvalue of t_lvalue
  | ILit of int
  | FLit of float
  | BLit of bool
  | RLit of char
  | SLit of string
  | Uexp of unop * t_rec
  | Bexp of binop * t_rec * t_rec
  | Fn_call of t_lvalue * t_rec list
  | Append of id * t_rec
and t_expr = { exp : t_rec; typ : typ; }

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
(*
type expr =
  | Iden of id
  | Array_expr of expr * expr
  | Struct_expr of expr * expr
  | ILit of int
  | FLit of float
  | BLit of bool
  | RLit of char
  | SLit of string
  | Uexp of unop * expr
  | Bexp of binop * expr * expr
  | Fn_call of lvalue * expr list
  | Append of id * expr

type lvalue = 
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
*)




(* Annotated tree *)
type ('a, 'e, 'i) annotated_lvalue = 
  | LIden of 'i * 'a
  | LAValue of ('a, 'e, 'i) annotated_lvalue * 'e * 'a
  | LSValue of ('a, 'e, 'i) annotated_lvalue * 'i * 'a

type 'i annotated_typ =
  | TSimp   of 'i (*typ_id*)
  | TStruct of ('i (*typ_id*) * 'i annotated_typ) list
  | TArray  of 'i annotated_typ * int
  | TSlice  of 'i annotated_typ
  | Void

type ('a, 'e, 'i) annotated_expr =
(*  | Iden of id * 'a
  | AValue of 'e * 'e * 'a
  | SValue of 'e * 'e * 'a
*)
  | LValue of ('a, 'e, 'i) annotated_lvalue
  | ILit of int * 'a
  | FLit of float * 'a
  | BLit of bool * 'a
  | RLit of char * 'a
  | SLit of string * 'a
  | Uexp of unop * 'e * 'a
  | Bexp of binop * 'e * 'e * 'a
  | Fn_call of ('a, 'e, 'i) annotated_lvalue * 'e list * 'a
  | Append of 'i * 'e * 'a

type ('a, 'e, 'i) annotated_assignment = ('a, 'e, 'i) annotated_lvalue list * 'e list * 'a

(* Statements *)
type ('a, 's, 'e, 'i) annotated_stmt =
  | Assign  of ('a, 'e, 'i) annotated_assignment
  | Print   of 'e list * 'a
  | Println of 'e list * 'a
  | If_stmt of 's option * 'e * 's list * 's list option * 'a
  | Switch_stmt of 's option * 'e option * 's list * 'a
  | Switch_clause of 'e list option * 's list * 'a
  | For_stmt    of 's option * 'e option * 's option * 's list * 'a
  | Var_stmt    of ('i list * 'e list option * 'i annotated_typ option) list * 'a
  | SDecl_stmt  of 'i list * 'e list option * 'a
  | Type_stmt   of ('i * 'i annotated_typ) list * 'a
  | Expr_stmt   of 'e * 'a
  | Return      of 'e option * 'a
  | Block       of 's list * 'a
  | Break       of 'a
  | Continue    of 'a
  | Empty_stmt  of 'a

(* Top-level declarations *)
type ('a, 's, 'e, 'i) annotated_decl =
  | Var_decl  of ('i list * 'e list option * 'i annotated_typ option) list * 'a
  | Type_decl of ('i * 'i annotated_typ) list * 'a
  | Func_decl of 'i * ('i * 'i annotated_typ) list * 'i annotated_typ * 's list * 'a

type ('d, 'i) annotated_ast = Prog of 'i * 'd list


(* Untyped *)
module Untyped = struct
  module Info = struct
    type t =
      {
        pos: Lexing.position;
      }
  end
  type info = Info.t (*{bline:int; bcol:int}*)
  type id   = string * info
  type expr = (info, expr, id) annotated_expr
  type stmt = (info, stmt, expr, id) annotated_stmt
  type decl = (info, stmt, expr, id) annotated_decl
  type ast  = (decl, id) annotated_ast
end


(* Typed *)
(*
module Typed = struct
  module TInfo = struct
    type t =
      {
        pos: Lexing.position;
      }
  end
  module UTInfo = struct
    type t =
      {
        pos: Lexing.position;
        typ: typ
      }
  end 

  (* typed *)
  type tinfo  = TInfo.t
  type utinfo = UTInfo.t
  type tid  = string * tinfo
  type utid = string * utinfo
  type expr = (tinfo, expr, tid) annotated_expr
  
  (* untyped *)
  type stmt = (utinfo, stmt, expr, tid) annotated_stmt
  type decl = (utinfo, stmt, expr, tid) annotated_decl
  type ast  = (decl, utid) annotated_ast
end
*)

let check_balance (vars, exprs) pos =
  if List.length vars <> List.length exprs
  then Error.print_error pos "unbalanced variables and expressions"
