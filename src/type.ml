(* module Ctx = Map.Make(String) *)

open Ast
exception TypeError of string
exception DeclError of string

(* let makeContext decls =  *)
(*   List.fold_left (fun gam (Dec(k,v)) -> if Ctx.mem k gam *)
(*                 then raise (DeclError("Variable \""^k^"\" already declared")) *)
(*                 else (Ctx.add) k v gam) *)
(*      (Ctx.empty) decls *)
(* let symTable (Prog(decls,_)) = makeContext decls *)

let typeAST (Prog(pkg,decls)) =
  let rec thread f gamma = function (* map, but updated gamma is used for next element *)
    | [] -> []
    | x::xs -> let (d,g) = f gamma x in d::thread f g xs
  in
  let rec tTyp gamma = function
    | TSimp(typ_id) -> failwith "not implemented"
    | TStruct(x_typ_ls) -> failwith "not implemented"
    | TArray(typ,d) -> failwith "not implemented"
    | TSlice(typ) -> failwith "not implemented"
    | Void -> failwith "not implemented"
  in
  let rec tExpr gamma = function
    | Lvalue(l) -> failwith "not implemented"
       (* let tl = tLVal gamma l in *)
       (*             { exp = Lvalue tl.exp ; typ = tl.typ } *)
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
  and tLVal gamma = function
    | Iden(id) -> failwith "not implemented"
    | AValue(r,e) -> failwith "not implemented"
    | SValue(r,id) -> failwith "not implemented"

  in
  let rec tStmt gamma = function
    | Assign(xs, es) -> failwith "not implemented"
       (* if not (Ctx.mem id gamma) *)
       (* then raise (DeclError("Assignment of undeclared variable \""^id^"\"")) *)
       (* else  *)
       (*   let tid = Ctx.find id gamma in *)
       (*   let te = typeExpr gamma e in *)
       (*   if (tid = te.typ) || (tid = TFloat && te.typ = TInt) *)
       (*   then Assign(id, te) *)
       (*   else raise (TypeError "Mismatch in assignment") *)
    | Print(es) -> Print(List.map (tExpr gamma) es) (* change tExpr to return a pair and use thread instead of map? *)
    | Println(es) -> Println(List.map (tExpr gamma) es)
    | If_stmt(po,e,ps,pso) -> failwith "not implemented"
    | Switch_stmt(po, eo, ps) -> failwith "not implemented"
    | Switch_clause(eso, ps) -> failwith "not implemented"
    | For_stmt(po1, eo, po2, ps) -> failwith "not implemented"
    | Var_stmt(ids_eso_typo_ls) -> failwith "not implemented"
    | SDecl_stmt(ids, eso) -> failwith "not implemented"
    | Type_stmt(id_typ_ls) -> failwith "not implemented"
    | Expr_stmt e -> failwith "not implemented"
    | Return(eo) -> failwith "not implemented"
    | Break -> failwith "not implemented"
    | Continue -> failwith "not implemented"
    | Empty_stmt -> failwith "not implemented"
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
  let rec tDecl gamma = function
    | Var_decl(ids_eso_typo_ls) -> failwith "not implemented"
    | Type_decl(typId_typ_ls) -> failwith "not implemented"
    | Func_decl(fId, id_typ_ls, typ, ps) -> failwith "not implemented"
  and tDecls gamma ds = thread tDecl gamma ds

  in
  TProg(pkg, tDecls Ctx.empty decls)
