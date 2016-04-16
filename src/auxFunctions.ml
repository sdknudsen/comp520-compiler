open Ast
open Context

let rec sTyp (at:Typed.uttyp) =
  match at with
  | TSimp(("int",g))
    | TSimp(("float64",g))
    | TSimp(("string",g))
    | TSimp(("rune",g))
    | TSimp(("bool",g)) -> at
  | TSimp((x,g)) ->
     (match find x g with
      | Some(TKind(x)) -> sTyp x
      | _ -> failwith "Not valid type type")
  (* | t -> Some(t) *)
  | TStruct(x_typ_ls) -> TStruct(List.map (fun(id,typ) -> id, sTyp typ) x_typ_ls)
  | TArray(typ,d) -> TArray(sTyp typ,d)
  | TSlice(typ) -> TSlice(sTyp typ)
  | TVoid -> TVoid
  | TFn(a,b) -> TFn(List.map sTyp a,sTyp b)
  | TKind(a) -> TKind(sTyp a)

let rec sTyp2 (at:Typed.uttyp) =
  match at with
  | TSimp(("int",g))
    | TSimp(("float64",g))
    | TSimp(("string",g))
    | TSimp(("rune",g))
    | TSimp(("bool",g)) -> at
  | TSimp((x,g)) ->
     (match find x g with
      | Some(TKind(x)) -> sTyp2 x
      | _ -> failwith "Not valid type type")
      (* | None -> failwith "Not valid type type") *)
  (* | t -> Some(t) *)
  | TStruct(x_typ_ls) -> TStruct(List.map (fun(id,typ) -> id, sTyp2 typ) x_typ_ls)
  | TArray(typ,d) -> TArray(sTyp2 typ,d)
  | TSlice(typ) -> TSlice(sTyp2 typ)
  | TVoid -> TVoid
  | TFn(a,b) -> TFn(List.map sTyp2 a,sTyp2 b)
  | TKind(a) -> sTyp2 a

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

let all_same f = function
  | [] -> true
  | x::xs -> List.for_all (fun y -> f y == f x) xs

let mapo f o = match o with
  | None -> None
  | Some x -> Some (f x)

let rec list_type = function
  | [] -> failwith "empty list"
  | [x] -> x
  | x::y::tl -> if x = y then list_type (y::tl)
                else failwith "Multiple types in single assignment"

let rec zip l1 l2 = match (l1,l2) with
  | (x::xs, y::ys) -> (x,y)::zip xs ys
  | ([],[]) -> []
  | _ -> failwith "Mismatch on number of arguments"

let rec unzip l = match l with
  | (x,y)::tl -> let (xs,ys) = unzip tl in (x::xs,y::ys)
  | [] -> ([],[])

let rec get_base_type = function
   | TSimp((x,g)) ->
       (match find x g with
         | Some(TKind(x)) -> get_base_type x
         | _ -> None)
   | t -> Some(t)

let rec same_type t1 t2 = 
   match t1, t2 with
    | TVoid, TVoid -> true
    | TSimp((a,c)), TSimp((a',c')) -> a = a' && c == c'
    | TKind(t), TKind(t') -> same_type t t'
    | TArray(t, i), TArray(t', i') -> same_type t t' && i = i'
    | TSlice(t), TSlice(t') -> same_type t t'
    | TStruct(tl), TStruct(tl') ->
        List.exists2 (fun (i,t) (i',t') -> not (i = i' && (same_type t t'))) tl tl'
    | _, _ -> false

let rec isBool = function
    | TSimp(("bool", _)) -> true
    | TSimp(("#", _)) -> false
    | TSimp((x,g)) ->
        (match find x g with
         | Some(TKind(x)) -> isBool x
         | _ -> false)
    | _ -> false

let rec isCastable = function
    | TSimp(("bool", _))
    | TSimp(("int", _))
    | TSimp(("rune", _))
    | TSimp(("float64", _)) -> true
    | TSimp(("#", _)) -> false
    | TSimp((x,g)) ->
        (match find x g with
         | Some(TKind(x)) -> isCastable x
         | _ -> false)
    | _ -> false

let isBaseType = function
    | TSimp((x, g)) ->
        (match find x g with
          | Some(TKind(TSimp("#", _))) -> true
          | _ -> false)
    | _ -> false

let rec isComparable = function
    | TSimp(("bool", _))
    | TSimp(("int", _))
    | TSimp(("float64", _))
    | TSimp(("string", _))
    | TSimp(("rune", _)) -> true
    | TSimp(("#", _)) -> false
    | TStruct(fds) -> List.for_all (fun (i,t) -> isComparable t) fds
    | TArray(t, d) -> (isComparable t)
    | TSimp((x,g)) ->
        (match find x g with
         | Some(TKind(x)) -> isComparable x
         | _ -> false)
    | _ -> false

let rec isOrdered = function
    | TSimp(("int",_))
    | TSimp(("float64",_))
    | TSimp(("string",_))
    | TSimp(("rune",_)) -> true
    | TSimp(("#", _)) -> false
    | TSimp((x,g)) ->
        (match find x g with
         | Some(TKind(x)) -> isOrdered x
         | _ -> false)
    | _ -> false

let rec isNumeric = function
    | TSimp(("int",_))
    | TSimp(("float64",_))
    | TSimp(("rune",_)) -> true
    | TSimp(("#", _)) -> false
    | TSimp((x,g)) ->
        (match find x g with
         | Some(TKind(x)) -> isNumeric x
         | _ -> false)
    | _ -> false

let rec isInteger  = function
    | TSimp(("int",_))
    | TSimp(("rune",_)) -> true
    | TSimp(("#", _)) -> false
    | TSimp((x,g)) ->
        (match find x g with
         | Some(TKind(x)) -> isInteger x
         | _ -> false)
    | _ -> false

let rec isString = function
    | TSimp(("string",_)) -> true
    | TSimp(("#", _)) -> false
    | TSimp((x,g)) ->
        (match find x g with
         | Some(TKind(x)) -> isString x
         | _ -> false)
    | _ -> false
