open Ast

let uop_to_str = function
  | Positive -> "+"           
  | Negative -> "-"
  | Boolnot -> "!"
  | Bitnot -> "^"

let bop_to_str = function
  | Boolor -> "||"
  | Booland -> "&&"
  | Equals -> "=="
  | Notequals -> "!="
  | Lt -> "<"
  | Lteq -> "<="
  | Gt -> ">"
  | Gteq -> ">="
  | Plus -> "+"
  | Minus -> "-"
  | Bitor -> "|"
  | Bitand -> "&"
  | Bitnand -> "&^"
  | Bitxor -> "^"
  | Times -> "*"
  | Div -> "/"
  | Modulo -> "%"
  | Lshift -> "<<"
  | Rshift -> ">>"

let rec lv_to_str = function
  | Iden(id) -> id
  | AValue(t_lvalue, t_expr) -> lv_to_str t_lvalue
  | SValue(t_lvalue, id) -> lv_to_str t_lvalue

let typ_to_str = function
  | TSimp(x) -> x
  | TStruct(id_typ_ls) -> failwith "not done"
  | TArray(t,d) -> failwith "not done"
  | TSlice(t) -> failwith "not done"
  | Void -> failwith "not done"

let kind_to_str = function
  | Var -> "var"
  | Typ -> "type"
  | Fun -> "func"

let isBool g = function
  | TSimp "bool" -> true
  (* also, check if it's a new type in the context *)
  | _ -> false

let isComparable g = function
  | _ -> false

let isOrdered g = function
  | _ -> false

let isNumeric g = function
  | TSimp "int" -> true
  | TSimp "rune" -> true
  | TSimp "float64" -> true
  | _ -> false

let isInteger g = function
  | TSimp "int" -> true
  | TSimp "rune" -> true
  | _ -> false

let isString g = function
  | TSimp "string" -> true
  | _ -> false
