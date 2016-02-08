module Ctx = Map.Make(String)
exception TypeError of string
exception DeclError of string

type id = string
type typ = TInt
   | TString
   | TFloat

type binop = PLUS | MINUS | TIMES | DIV
type unop = NEG
(* type info =  *)

type 'a exprF = ILit of int
        | FLit of float
        | SLit of string
        | Var of string
        | Bexp of binop * 'a * 'a
        | Uexp of unop * 'a
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

let typOf x = snd x

let str_of_binop = function
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | DIV -> "/"

let str_of_unop = function
  | NEG -> "-"
    
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
      | Bexp(op,e1,e2) ->
         let te1 = typeExpr gamma e1 in 
         let te2 = typeExpr gamma e2 in 
         let t = (match (typOf te1, typOf te2, op) with
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
         (Uexp(NEG,te), typOf te)
  in
  let rec typeStmt = function
    | Assign(id,e) ->
       if not (Ctx.mem id gamma)
       then raise (DeclError("Assignment of undeclared variable \""^id^"\""))
       else 
         let tid = Ctx.find id gamma in
         let te = typeExpr gamma e in
         if (tid = typOf te) || (tid = TFloat && typOf te = TInt)
         then Assign(id, te)
         else raise (TypeError "Mismatch in assignment")
    | Print(e) -> Print(typeExpr gamma e)
    | Read(id) -> if Ctx.mem id gamma
                  then Read(id)
                  else raise (DeclError ("Read of undeclared variable \""^id^"\""))
    | Ifte(e,xs,ys) ->
       let te = typeExpr gamma e in
       if typOf te = TInt
       then Ifte(typeExpr gamma e, typeStmts xs, typeStmts ys)
       else raise (TypeError "Expression in if-then-else statement must have int type")
    | Ift(e,xs) ->
       let te = typeExpr gamma e in
       if typOf te = TInt
       then Ift(typeExpr gamma e, typeStmts xs)
       else raise (TypeError "Expression in if-then statement must have int type")
    | While(e,xs) ->
       let te = typeExpr gamma e in
       if typOf te = TInt
       then While(te, typeStmts xs)
       else raise (TypeError "Expression in while statement must have int type")
  and typeStmts xs = List.map typeStmt xs in
  TProg(decls, typeStmts stmts)
