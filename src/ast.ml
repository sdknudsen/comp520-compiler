type id = string
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
  (* | Parens  of 'e *)
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
