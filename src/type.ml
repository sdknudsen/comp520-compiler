(* module Ctx = Map.Make(String) *)

open Ast
open Context
exception TypeError of string
exception DeclError of string

let str_of_lv = function
  | Iden(id) -> id
  | AValue(t_lvalue, t_expr) -> failwith "not done"
  | SValue(t_lvalue, id) -> failwith "not done"

(* type context = lvalue Ast.Ctx.t *)

(* type context = string Ast.Ctx.t *)

(* let makeContext decls =  *)
(*   List.fold_left (fun gam (Dec(k,v)) -> if Ctx.mem k gam *)
(*                 then raise (DeclError("Variable \""^k^"\" already declared")) *)
(*                 else (Ctx.add) k v gam) *)
(*      (Ctx.empty) decls *)
(* let symTable (Prog(decls,_)) = makeContext decls *)
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
  | _ -> []

let rec unzip l = match l with
  | (x,y)::tl -> let (xs,ys) = unzip tl in (x::xs,y::ys)
  | [] -> ([],[])

let typeAST (Prog(pkg,decls)) =
  (* I think this is going to have to return a pair at the end *)
  let rec thread f gamma = function (* map, but updated gamma is used for next element *)
    | [] -> []
    | x::xs -> let (d,g) = f gamma x in d::thread f g xs
  in
  (* let rec tExpr gamma = function *)
  let rec tExpr g e : t_expr = match e with
    | Lvalue(l) -> failwith "not implemented"
       (* let tl = tLVal g l in *)
       (* { exp = Lvalue tl.exp ; typ = tl.typ } *)

    | ILit(d) -> { exp = ILit d ; typ = TSimp "int" }
    | FLit(f) -> { exp = FLit f ; typ = TSimp "float" }
    | BLit(b) -> { exp = BLit b ; typ = TSimp "bool" }
    | RLit(c) -> { exp = RLit c ; typ = TSimp "rune" }
    | SLit(s) -> { exp = SLit s ; typ = TSimp "string" }
    | Bexp(op,e1,e2) -> failwith "not implemented"
       (* let te1 = typeExpr gamma e1 in  *)
       (* let te2 = typeExpr gamma e2 in  *)
       (* let t = (match (te1.typ, te2.typ, op) with *)
       (*          | TInt, TInt, _ -> TInt *)
       (*          | TInt, TFloat, _ -> TFloat *)
       (*          | TFloat, TInt, _ -> TFloat *)
       (*          | TFloat, TFloat, _ -> TFloat *)
       (*          | TString, TString, PLUS -> TString *)
       (*          | TString, TString, MINUS -> TString *)
       (*          | _ -> raise (TypeError ("Mismatch with '" ^ str_of_binop op ^ "' operation"))) *)
       (* in { exp = Bexp(op,te1,te2) ; typ = t } *)
    | Uexp(op,e) -> failwith "not implemented"
       (* let te = typeExpr gamma e *)
       (* in { exp = Uexp(op,te) ; typ = t }  *)
    | Fn_call(fun_id, es) -> failwith "not implemented"
    | Append(x, e) -> failwith "not implemented"
  and tLVal g l : t_lvalue = match l with
    | Iden(id) -> failwith "not implemented"
    | AValue(r,e) -> failwith "not implemented"
    | SValue(r,id) -> failwith "not implemented"

  in
  let get_assign_typ g (lv,e) = 
    let t_lv = tLVal g lv in
    (* fix t_lv problems, currently, it just compares with the string for Id and rejects for other lvalues *)
    let id = str_of_lv lv in
    if not (mem id g)
    then raise (DeclError("Assignment of undeclared variable \""^id^"\""))
    else let tid = find id g in
         let te = tExpr g e in
         if (tid = te.typ) (*|| (tid = TFloat && te.typ = TInt) add parametricity!*)
         then (t_lv,te)
         else raise (TypeError "Mismatch in assignment")
  in
  (* let rec tStmt gamma = function *)
  let rec tStmt g p : t_stmt * context = match p with
    | Assign(xs,es) -> 
       let (txs, tes) = unzip (List.map (get_assign_typ g) (zip xs es)) in
       (Assign(txs, tes), g)

    | Print(es) -> (Print(List.map (tExpr g) es), g) (* change tExpr to return a pair and use thread instead of map? *)
    | Println(es) -> (Println(List.map (tExpr g) es), g)
    | If_stmt(po,e,ps,pso) ->
       (If_stmt(mapo (fun p -> fst (tStmt g p)) po, (* does using fst solve everything here? *)
               tExpr g e,
               thread tStmt g ps,
               mapo (thread tStmt g) pso), g)

    | Switch_stmt(po, eo, ps) -> failwith "not implemented"
    | Switch_clause(eso, ps) -> failwith "not implemented"
    | For_stmt(po1, eo, po2, ps) -> failwith "not implemented"
    | Var_stmt(ids_eso_typo_ls) -> failwith "not implemented"
    | SDecl_stmt(ids, eso) -> failwith "not implemented"
    | Type_stmt(id_typ_ls) -> failwith "not implemented"
    | Expr_stmt e -> failwith "not implemented"
    | Return(eo) -> failwith "not implemented"
    | Break -> (Break, g)
    | Continue -> (Continue, g)
    | Empty_stmt -> (Empty_stmt, g)
(* and tStmts xs = List.map tStmt xs in *)

(*
  let rec typeStmt = function
    | Read(id) -> if Ctx.mem id gamma
                  then Read(id)
                  else raise (DeclError ("Read of undeclared variable \""^id^"\""))
    | Ifte(e,xs,ys) ->
       let te = typeExpr gamma e in
       if te.typ = TInt
       then Ifte(typeExpr gamma e, typeStmts xs, typeStmts ys)
       else raise (TypeError "Expression in if-then-else statement must have int type")
 *)

  in
  (* let rec tDecl gamma = function *)
  let rec tDecl g d : t_decl * context = match d with
    | Var_decl(ids_eso_typo_ls) -> failwith "not implemented"
       (* if Ctx.in_scope typ_id gamma then raise DeclError "Already declared in scope" *)
       (* else Ctx.add typ_id gamma *)
    | Type_decl(typId_typ_ls) -> failwith "not implemented"
    | Func_decl(fId, id_typ_ls, typ, ps) -> failwith "not implemented"
  and tDecls gamma ds = thread tDecl gamma ds
  in
  TProg(pkg, tDecls 
               (build_symtab decls stdout false)
               decls)

(* lvalues vs strings in the  context *)
(* why outc? *)

(* build_symtab (Prog(_,decls)) outc dumpsymtab =  *)

(* replace Ctx.empty with a context that already has bool, int, float, ... *)
(*
Types:
bool byte complex64 complex128 error float32 float64
int int8 int16 int32 int64 rune string
uint uint8 uint16 uint32 uint64 uintptr
Constants:
true false iota
Zero value: nil
Functions:
append cap close complex copy delete imag len make new panic print println real recover
*)

  (* let rec tTyp gamma = function *)
  (*   | TSimp(typ_id) -> failwith "not implemented" *)
  (*      (\* if Ctx.mem typ_id gamma then  *\) *)
  (*      (\* if Ctx.in_scope typ_id gamma then raise DeclError "Already declared in scope" *\) *)
  (*      (\* else Ctx.add typ_id gamma *\) *)
  (*   | TStruct(x_typ_ls) -> failwith "not implemented" *)
  (*   | TArray(typ,d) -> failwith "not implemented" *)
  (*   | TSlice(typ) -> failwith "not implemented" *)
  (*   | Void -> failwith "not implemented" *)
  (* in *)

    (* | ILit(d) -> ILit(d) *)
    (* | FLit(f) -> FLit(f) *)
    (* | BLit(b) -> BLit(b) *)
    (* | RLit(c) -> RLit(c) *)
    (* | SLit(s) -> SLit(s) *)
