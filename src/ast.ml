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

(* Annotated tree *)
type ('i, 't) annotated_typ =
  | TSimp   of 't
  | TStruct of ('i * ('i, 't) annotated_typ) list
  | TArray  of ('i, 't) annotated_typ * int
  | TSlice  of ('i, 't) annotated_typ
  | TFn     of ('i, 't) annotated_typ list * ('i, 't) annotated_typ
  | TKind   of ('i, 't) annotated_typ
  | TVoid

type ('e, 'i, 't) annotated_expr =
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

type ('s, 'e, 'i, 't) annotated_stmt =
  | Assign      of 'e list * 'e list
  | If_stmt     of 's option * 'e * 's list * 's list option
  | Switch_stmt of 's option * 'e option * 's list
  | For_stmt    of 's option * 'e option * 's option * 's list
  | Switch_clause of 'e list option * 's list
  | Var_stmt    of ('i * 'e option * 't option) list list
  | SDecl_stmt  of ('i * 'e) list
  | Type_stmt   of ('i * 't) list
  | Return      of 'e option
  | Block       of 's list
  | Print       of 'e list
  | Println     of 'e list
  | Expr_stmt   of 'e
  | Break
  | Continue
  | Empty_stmt

type ('s, 'e, 'i, 't) annotated_decl =
  | Var_decl  of ('i * 'e option * 't option) list list
  | Type_decl of ('i * 't) list
  | Func_decl of 'i * ('i * 't) list * 't * 's list

type ('d, 'i) annotated_ast = Prog of 'i * 'd list


(* Untyped *)
module Untyped = struct
  type id = string * Lexing.position
  type uttyp = (id, id) annotated_typ
  type utexpr = (annotated_utexpr, id, uttyp) annotated_expr
  and annotated_utexpr =  (utexpr * Lexing.position)
  type utstmt = (annotated_utstmt, annotated_utexpr, id, uttyp) annotated_stmt
  and annotated_utstmt = utstmt * Lexing.position
  type utdecl = (annotated_utstmt, annotated_utexpr, id, uttyp) annotated_decl
  type annotated_utdecl = utdecl * Lexing.position
  type ast  = (annotated_utdecl, id) annotated_ast
end

module Typed = struct
  type id = string
  (*type uttyp '= (string * Context.context) annotated_typ*)
  type uttyp = (string, string) annotated_typ
  type texpr = (annotated_texpr, id, uttyp) annotated_expr
  and annotated_texpr =  (texpr * (Lexing.position * uttyp))
  type utstmt = (annotated_utstmt, annotated_texpr, id, uttyp) annotated_stmt
  and annotated_utstmt = utstmt * Lexing.position
  type utdecl = (annotated_utstmt, annotated_texpr, id, uttyp) annotated_decl
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



(* (\* Annotated with records *\) *)
(* type annotation = () (\**** change this line ****\) *)
(* type a_expr_rec = *)
(*   (\* | Lvalue of a_lvalue *\) *)
(*   | ILit of int *)
(*   | FLit of float *)
(*   | BLit of bool *)
(*   | RLit of char *)
(*   | SLit of string *)
(*   | Uexp of unop * a_expr *)
(*   | Bexp of binop * a_expr * a_expr *)
(*   | Fn_call of a_lvalue * a_expr list *)
(*   | Append of id * a_expr *)
(* and a_expr = { exp : a_expr_rec ; expr_ann : annotation } *)

(* and a_lvalue_rec = *)
(*   | Iden of id *)
(*   | AValue of a_lvalue * a_expr *)
(*   | SValue of a_lvalue * id *)
(* and a_lvalue = { lval : a_lvalue_rec ; lval_ann : annotation } *)

(* type a_assignment_rec = a_lvalue list * a_expr list *)
(* and a_assignment = { assign : a_assignment_rec ; assign_ann : annotation } *)

(* type a_stmt_rec = *)
(*   | Assign  of a_assignment *)
(*   | Print   of a_expr list *)
(*   | Println of a_expr list *)
(*   | If_stmt of a_stmt option * a_expr * a_stmt list * a_stmt list option *)
(*   | Switch_stmt of a_stmt option * a_expr option * a_stmt list *)
(*   | Switch_clause of a_expr list option * a_stmt list *)
(*   | For_stmt    of a_stmt option * a_expr option * a_stmt option * a_stmt list *)
(*   | Var_stmt    of (id list * a_expr list option * typ option) list *)
(*   | SDecl_stmt  of (id list * a_expr list option) *)
(*   | Type_stmt   of (id * typ) list *)
(*   | Expr_stmt   of a_expr *)
(*   | Return      of a_expr option *)
(*   | Break *)
(*   | Continue *)
(*   | Empty_stmt *)
(* and a_stmt = { stmt : a_stmt_rec ; stmt_ann : annotation } *)

(* type a_decl_rec = *)
(*   | Var_decl  of (id list * a_expr list option * typ option) list *)
(*   | Type_decl of (typ_id * typ) list *)
(*   | Func_decl of fun_id * (id * typ) list * typ * a_stmt list *)
(* and a_decl = { decl : a_decl_rec ; decl_ann : annotation } *)

(* type a_ast = TProg of pkg_id * a_decl list *)
