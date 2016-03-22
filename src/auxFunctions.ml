open Ast
open Errors
open Context

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

let rec lv_to_str (tlv:t_lvalue) = match tlv with
  | Iden(id) -> id
  | AValue(t_lvalue, t_expr) -> lv_to_str t_lvalue
  | SValue(t_lvalue, id) -> lv_to_str t_lvalue

let mapo f o = match o with
  | None -> None
  | Some x -> Some (f x)

let typo f gam o = match o with
  | None -> (None, gam)
  | Some x -> let (tx,g) = f gam x in (Some(tx), g)

let rec list_type = function
  | [] -> failwith "empty list"
  | [x] -> x
  | x::y::tl -> if x = y then list_type (y::tl)
                else raise (TypeError ("Multiple types in single assignment"))

let rec zip l1 l2 = match (l1,l2) with
  | (x::xs, y::ys) -> (x,y)::zip xs ys
  | ([],[]) -> []
  | _ -> failwith "Mismatch on number of arguments"

let rec unzip l = match l with
  | (x,y)::tl -> let (xs,ys) = unzip tl in (x::xs,y::ys)
  | [] -> ([],[])

let rec getTypName (at : string annotated_typ) : string = match at with
  | TSimp(i) -> i
  | TStruct(i) -> failwith "Simple type required"
    (* of ('i * 'i annotated_typ) list *)
  | TArray(i,d) -> failwith "Simple type required"
    (* of 'i annotated_typ * int *)
  | TSlice(i) -> failwith "Simple type required"
    (* of 'i annotated_typ *)
  | Void -> failwith "Simple type required"
  (* is anything other than tsimp allowd for type declarations? *)

let rec getBaseTyp g t : (string annotated_typ) =
  let s = typ_to_str t in
  try let (kind,at) = find s g
      in match kind with
         | Typ -> getBaseTyp g at
         | _ -> t
  with
  | _ -> t
  
let unify g ta tb = 
  let t1 = getBaseTyp g ta in
  let t2 = getBaseTyp g tb in
  if t1 == t2 then t1 else 
    raise (TypeError ("Types " ^ (typ_to_str t1) ^ " and " ^ (typ_to_str t2) ^ " do not unify"))

let isBool t =
    t = TSimp "bool"

let isComparable t =
    match t with
    | _ -> false

let isOrdered t =
    match t with
    | _ -> false

let isNumeric t =
  t = TSimp "int" || t = TSimp "rune" || t = TSimp "float64"
  (* match t with *)
  (* | TSimp "int" *)
  (*   | TSimp "rune" *)
  (*   | TSimp "float64" -> true *)
  (* | _ -> false *)

let isInteger t =
  (* if e1 != e2 then false else *)
  (*   match e1 with *)
  (*   | TSimp "int" -> true *)
  (* | TSimp "rune" -> true *)
  (* | _ -> false *)
           t = TSimp "int" || t = TSimp "rune"

let isString t =
  t = TSimp "string"
