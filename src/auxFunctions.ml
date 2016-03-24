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

(* let rec lv_to_str (tlv:t_lvalue) = match tlv with *)
(*
let rec lv_to_str = function
  | Iden(id) -> id
  | AValue(t_lvalue, t_expr) -> lv_to_str t_lvalue
  | SValue(t_lvalue, id) -> lv_to_str t_lvalue
*)

(*
let typ_to_str = function
  | TSimp(x) -> x
  | TStruct(id_typ_ls) -> failwith "not done"
  | TArray(t,d) -> failwith "not done"
  | TSlice(t) -> failwith "not done"
  | Void -> failwith "not done"
*)

let mapo f o = match o with
  | None -> None
  | Some x -> Some (f x)

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

(*
let rec getBaseTyp g t =
  match t with
   | TSimp x -> 
      match g with
        | Root(tbl) ->
           match Hashtable.find tbl x with
             | 
             | _ -> 
        | Frame(tbl, ctx) 

  match g with

  try
    match find t g with
     | (Context.Typ,t') -> getBaseTyp g t'
     | _ -> t'
  with
  | _ -> t

let unify g ta tb = 
  let t1 = getBaseTyp g ta in
  let t2 = getBaseTyp g tb in
  if t1 == t2 then t1 else 
    raise (TypeError ("Types " ^ typ_to_str t1 ^ " and " ^ typ_to_str t2 ^ " do not unify"))
*)

let isBaseType t = 
   t = TSimp "int" 
|| t = TSimp "rune"
|| t = TSimp "float64"
|| t = TSimp "string"
|| t = TSimp "bool"

let rec same_type t1 t2 = match t1, t2 with
    | TVoid, TVoid -> true
    | TSimp(a), TSimp(a') -> a = a'
    | TKind(t), TKind(t') -> same_type t t'
    | TArray(t, i), TArray(t', i') -> same_type t t' && i = i'
    | TSlice(t), TSlice(t') -> same_type t t'
    | TStruct(tl), TStruct(tl') -> false
    | _, _ -> false

let isBool t =
    t = TSimp "bool"

let rec isComparable t =
    match t with
    | TSimp("bool")
    | TSimp("int")
    | TSimp("float64")
    | TSimp("string")
    | TSimp("rune") -> true
    | TStruct(fds) -> List.for_all (fun (i,t) -> isComparable t) fds
    | TArray(v, d) -> (isComparable t)
    | _ -> false

let isOrdered t =
    match t with
    | TSimp("int")
    | TSimp("float64")
    | TSimp("string")
    | TSimp("rune") -> true
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

  (* let rec thread f gamma = function (\* map, but updated gamma is used for next element *\) *)
  (*   | [] -> ([],gamma) *)
  (*   | x::xs -> let (d,g) = f gamma x in *)
  (*              let (tl,gam) = thread f g xs in *)
  (*              (d::tl,gam) *)
  (* in *)

