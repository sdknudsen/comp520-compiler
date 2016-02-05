module Ctx = Map.Make(String)
exception TypeError of string
exception DeclError of string

type id = string
type typ = TInt
	 | TString
	 | TFloat

type 'a exprF = ILit of int
	      | FLit of float
	      | SLit of string
	      | Var of string
	      | Sum of 'a * 'a
	      | Diff of 'a * 'a
	      | Prod of 'a * 'a
	      | Quot of 'a * 'a
	      | Neg of 'a
type expr = expr exprF
type t_expr = (t_expr * typ) exprF

type ('e,'s) stmtF = Assign of id * 'e
		   | Print of 'e
		   | Read of id
		   | Ifte of 'e * 's list * 's list
		   | Ift of 'e * 's list 
		   | While of 'e * 's list 
type stmt = (expr,stmt) stmtF
type t_stmt = (t_expr * typ, t_stmt) stmtF

type declaration = Dec of id * typ

type ast = Prog of declaration list * stmt list
type t_ast = TProg of declaration list * t_stmt list


let makeContext decls = 
  List.fold_left (fun gam (Dec(k,v)) -> if Ctx.mem k gam
				        then raise (DeclError("Variable \""^k^"\" already declared"))
				        else (Ctx.add) k v gam)
		 (Ctx.empty) decls

let symTable (Prog(decls,_)) = makeContext decls

let typeAST (Prog(decls,stmts)) =
  let gamma = makeContext decls in
  let rec typeExpr gamma = function
      | ILit(v) -> (ILit v, TInt)
      | FLit(v) -> (FLit v, TFloat)
      | SLit(v) -> (SLit v, TString)
      | Var(x) ->
         if Ctx.mem x gamma
         then Var(x), Ctx.find x gamma
         else raise (DeclError("Expression contains undeclared variable \""^x^"\""))
      | Sum(e1,e2) -> let (te1,te2,t) = typeHelper e1 e2 "+" in (Sum(te1,te2), t)
      | Diff(e1,e2) -> let (te1,te2,t) = typeHelper e1 e2 "-" in (Diff(te1,te2), t)
      | Prod(e1,e2) -> let (te1,te2,t) = typeHelper e1 e2 "*" in (Prod(te1,te2), t)
      | Quot(e1,e2) -> let (te1,te2,t) = typeHelper e1 e2 "/" in (Quot(te1,te2), t)
      | Neg(e) -> let te = typeExpr gamma e in (Neg(te), snd te)
  and typeHelper e1 e2 op =
    let te1 = typeExpr gamma e1 in 
    let te2 = typeExpr gamma e2 in 
    let t = (match (snd te1, snd te2, op) with
	     | TInt, TInt, _ -> TInt
	     | TInt, TFloat, _ -> TFloat
	     | TFloat, TInt, _ -> TFloat
	     | TFloat, TFloat, _ -> TFloat
	     | TString, TString, "+" -> TString
	     | TString, TString, "-" -> TString
	     | _ -> raise (TypeError ("Mismatch with '" ^ op ^ "' operation")))
    in (te1,te2,t)
  in
  let rec typeStmt = function
    | Assign(id,e) ->
       if not (Ctx.mem id gamma)
       then raise (DeclError("Assignment of undeclared variable \""^id^"\""))
       else 
         let tid = Ctx.find id gamma in
         let te = typeExpr gamma e in
         if (tid = snd te) || (tid = TFloat && snd te = TInt)
         then Assign(id, te)
         else raise (TypeError "Mismatch in assignment")
    | Print(e) -> Print(typeExpr gamma e)
    | Read(id) -> if Ctx.mem id gamma
                  then Read(id)
                  else raise (DeclError ("Read of undeclared variable \""^id^"\""))
    | Ifte(e,xs,ys) ->
       let te = typeExpr gamma e in
       if snd te = TInt
       then Ifte(typeExpr gamma e, typeStmts xs, typeStmts ys)
       else raise (TypeError "Expression in if-then-else statement must have int type")
    | Ift(e,xs) ->
       let te = typeExpr gamma e in
       if snd te = TInt
       then Ift(typeExpr gamma e, typeStmts xs)
       else raise (TypeError "Expression in if-then statement must have int type")
    | While(e,xs) ->
       let te = typeExpr gamma e in
       if snd te = TInt
       then While(te, typeStmts xs)
       else raise (TypeError "Expression in while statement must have int type")
  and typeStmts xs = List.map typeStmt xs in
  TProg(decls, typeStmts stmts)
