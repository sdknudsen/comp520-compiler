(* removed rectypes for better error messages *)

(* module Ctx = Map.Make(String) *)
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

(*
(* Typed *)
type t_rec =
  | ILit of int
  | FLit of float
  | RLit of char
  | SLit of string
  | Parens of t_expr
  | Uexp of unop * t_expr
  | Bexp of binop * t_expr * t_expr
  | Fn_call of t_lvalue * t_expr list
  | Append of id * t_expr
and t_expr = { exp : t_rec ; typ : typ }

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
*)

(* Annotated tree *)
type 'i annotated_typ =
  | TSimp   of 'i
  | TStruct of ('i * 'i annotated_typ) list
  | TArray  of 'i annotated_typ * int
  | TSlice  of 'i annotated_typ
  | Void

type ('e, 'i) annotated_expr =
  | Iden    of 'i
  | Parens  of 'e
  | AValue  of 'e * 'e
  | SValue  of 'e * 'i
  | Append  of 'i * 'e
  | Fn_call of 'e * 'e list
  | ILit    of int
  | RLit    of char
  | FLit    of float
  | SLit    of string
  | Uexp    of unop * 'e
  | Bexp    of binop * 'e * 'e

type ('s, 'e, 'i) annotated_stmt =
  | Assign      of 'e list * 'e list
  | If_stmt     of 's option * 'e * 's list * 's list option
  | Switch_stmt of 's option * 'e option * 's list
  | For_stmt    of 's option * 'e option * 's option * 's list
  | Switch_clause of 'e list option * 's list
  | Var_stmt    of ('i list * 'e list option * 'i annotated_typ option) list
  | SDecl_stmt  of 'i list * 'e list option
  | Type_stmt   of ('i * 'i annotated_typ) list
  | Return      of 'e option
  | Block       of 's list
  | Print       of 'e list
  | Println     of 'e list
  | Expr_stmt   of 'e
  | Break
  | Continue
  | Empty_stmt

type ('s, 'e, 'i) annotated_decl =
  | Var_decl  of ('i list * 'e list option * 'i annotated_typ option) list
  | Type_decl of ('i * 'i annotated_typ) list
  | Func_decl of 'i * ('i * 'i annotated_typ) list * 'i annotated_typ * 's list

type ('d, 'i) annotated_ast = Prog of 'i * 'd list


(* Untyped *)
module Untyped = struct
  type id = string * Lexing.position
  type utexpr = (annotated_utexpr, id) annotated_expr
  and annotated_utexpr =  (utexpr * Lexing.position)
  type utstmt = (annotated_utstmt, annotated_utexpr, id) annotated_stmt
  and annotated_utstmt = utstmt * Lexing.position
  type utdecl = (annotated_utstmt, annotated_utexpr, id) annotated_decl
  type annotated_utdecl = utdecl * Lexing.position
  type ast  = (annotated_utdecl, id) annotated_ast
end

module Typed = struct
  type id = string * Lexing.position
  type texpr = (annotated_texpr, id) annotated_expr
  and annotated_texpr =  (texpr * (Lexing.position * string annotated_typ))
  type utstmt = (annotated_utstmt, annotated_texpr, id) annotated_stmt
  and annotated_utstmt = utstmt * Lexing.position
  type utdecl = (annotated_utstmt, annotated_texpr, id) annotated_decl
  type annotated_utdecl = utdecl * Lexing.position
  type ast  = (annotated_utdecl, id) annotated_ast
end

(************************************)
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
