(* module Ctx = Map.Make(String) *)

open Ast
exception TypeError of string
exception DeclError of string

let makeContext decls = 
  List.fold_left (fun gam (Dec(k,v)) -> if Ctx.mem k gam
                then raise (DeclError("Variable \""^k^"\" already declared"))
                else (Ctx.add) k v gam)
     (Ctx.empty) decls

(* let symTable (Prog(decls,_)) = makeContext decls *)

(* let typOf e = e.typ *)
(* let typOf x = snd x *)
(*
let typeAST Prog(stmts) =
  let gamma = Ctx.empty in
  let rec typeExpr gamma = function
    | ILit(v) -> { exp = ILit v ; try Ctx.find x gamma with _ -> raise (DeclError("Expression contains undeclared variable \""^x^"\""))}
      | FLit(v) -> { exp = FLit v ; TFloat)
      | BLit(v) -> { exp = SLit v ; TString)
      | RLit(v) -> { exp = SLit v ; TString)
      | SLit(v) -> { exp = SLit v ; TString)
      (* | TVar(x) -> *)
      | Var(x) ->
         if Ctx.mem x gamma
         then Var(x), Ctx.find x gamma
         else raise (DeclError("Expression contains undeclared variable \""^x^"\""))
      | Bexp(op,e1,e2) ->
         let te1 = typeExpr gamma e1 in 
         let te2 = typeExpr gamma e2 in 
         let t = (match (te1.typ, te2.typ, op) with
            | TInt, TInt, _ -> TInt
            | TInt, TFloat, _ -> TFloat
            | TFloat, TInt, _ -> TFloat
            | TFloat, TFloat, _ -> TFloat
            | TString, TString, PLUS -> TString
            | TString, TString, MINUS -> TString
            | _ -> raise (TypeError ("Mismatch with '" ^ str_of_binop op ^ "' operation")))
         in (Bexp(op,te1,te2), t)

      | Uexp(op,e) ->
         let te = typeExpr gamma e in
         (Uexp(NEG,te), te.typ)
  in
  let rec typeStmt = function
    | Assign(id,e) ->
       if not (Ctx.mem id gamma)
       then raise (DeclError("Assignment of undeclared variable \""^id^"\""))
       else 
         let tid = Ctx.find id gamma in
         let te = typeExpr gamma e in
         if (tid = te.typ) || (tid = TFloat && te.typ = TInt)
         then Assign(id, te)
         else raise (TypeError "Mismatch in assignment")
    | Print(e) -> Print(typeExpr gamma e)
    | Read(id) -> if Ctx.mem id gamma
                  then Read(id)
                  else raise (DeclError ("Read of undeclared variable \""^id^"\""))
    | Ifte(e,xs,ys) ->
       let te = typeExpr gamma e in
       if te.typ = TInt
       then Ifte(typeExpr gamma e, typeStmts xs, typeStmts ys)
       else raise (TypeError "Expression in if-then-else statement must have int type")
    | Ift(e,xs) ->
       let te = typeExpr gamma e in
       if te.typ = TInt
       then Ift(typeExpr gamma e, typeStmts xs)
       else raise (TypeError "Expression in if-then statement must have int type")
    | While(e,xs) ->
       let te = typeExpr gamma e in
       if te.typ = TInt
       then While(te, typeStmts xs)
       else raise (TypeError "Expression in while statement must have int type")
  and typeStmts xs = List.map typeStmt xs in
  TProg(decls, typeStmts stmts)
 *)
