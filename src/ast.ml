module Ctx = Map.Make(String)
exception TypeError of string
exception DeclError of string

type tm_id = string
type tp_id = string

type binop = Plus | Minus | Times | Div
type unop = Neg | Pos

(* type info =  *)

type 'a exprF = ILit of int
        | FLit of float
        | BLit of bool
        | RLit of char
        | SLit of string
        | Iden of id 
        | Bexp of binop * 'a * 'a
        | Uexp of unop * 'a
type expr = expr exprF

(* reverse t_rec and t_expr so that t_stmt doesn't need a type field? *)
type t_expr = t_rec exprF
and t_rec = { exp : t_expr; typ : tp_id; }
(*type t_expr = (t_expr * id) exprF*)


type 'e assignment = (id * 'e) list

type ('e,'s) stmtF = Assign of 'e assignment
       | Print of 'e
       | If_stmt of 'e * 's list * 's list option 
       | For_stmt of ('e * ('e assignment * 'e assignment) option) option
                     * 's list
       | Empty

type stmt = (expr, stmt) stmtF
(*type t_stmt = (t_expr * id, t_stmt) stmtF*)

type declaration = Dec of id * id

type ast = Prog of stmt list
(* type t_ast = TProg of t_stmt list *)





(*

let makeContext decls = 
  List.fold_left (fun gam (Dec(k,v)) -> if Ctx.mem k gam
                then raise (DeclError("Variable \""^k^"\" already declared"))
                else (Ctx.add) k v gam)
     (Ctx.empty) decls

let symTable (Prog(decls,_)) = makeContext decls

let typOf e = e.typ
(* let typOf x = snd x *)

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
