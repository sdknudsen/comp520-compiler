open Ast
open Parser
open Tokens
open AuxFunctions
open Ho

let getBase (Prog(id,decls) : Typed.ast) outc =
  let tabc = ref 0 in 
  let pln() = Printf.fprintf outc "\n" in 
  let pstr s = Printf.fprintf outc "%s" s in 
  let rec tabWith n = if n <= 0 then () else (pstr "\t"; tabWith (n-1)) in
  let tab() = tabWith !tabc in
  let pssl s f = 
    List.iter (fun y -> f y; pstr s)
  in
  let pcsl f = function 
    | [] -> ()
    | x::xs -> f x; List.iter (fun y -> pstr ", "; f y) xs
  in
  let plsl f = function 
    | [] -> ()
    | x::xs -> f x; List.iter (fun y -> pstr "\n"; f y) xs
  in
  let rec pTyp (at:Typed.uttyp) = match at with
    | TSimp(typ_id) -> 
    | TStruct(x_typ_ls) -> 
    | TArray(typ,d) -> 
    | TSlice(typ) -> 
    | TVoid -> 
    | TFn(a,b) -> 
    | TKind(a) -> 
  in
  
  let rec pExpr ((ue,(pos,typ,_)):Typed.annotated_texpr) = match ue with
    | Iden(id) -> 
    | AValue(r,e) -> 
    | SValue(r,id) -> 
    | ILit(d) -> 
    | FLit(f) -> 
    | RLit(c) -> 
    | SLit(s) -> 
    | Bexp(op,e1,e2) -> 
    | Uexp(op,e) -> 
    | Fn_call((Iden(i),_), k) -> 
    | Fn_call(fun_id, es) -> 
    | Append(x, e) -> 

  in
  let rec pStmt ((us, pos): Typed.annotated_utstmt) = match us with
    | Assign(xs, es) -> 
    | Var_stmt(xss) -> 
    | Print(es) -> 
    | Println(es) -> 
    | If_stmt(po,e,ps,pso) -> 
    | Block(stmts) -> 
    | Switch_stmt(po, eo, ps) -> 
    | Switch_clause(eso, ps) -> 
    | For_stmt(po1, eo, po2, ps) -> 
    | SDecl_stmt(id_e_ls) -> 
    | Type_stmt(id_typ_ls) -> 
    | Expr_stmt e -> 
    | Return(eo) -> 
    | Break -> 
    | Continue -> 
    | Empty_stmt -> 
  in
  let rec pDecl ((ud,pos): Typed.annotated_utdecl) = 
    match ud with
    | Var_decl(xss) -> 
    | Type_decl(id_atyp_ls) -> 
    | Func_decl(fId, id_typ_ls, typ, ps) -> 
  in
  (Prog(id, getDecl decls) : Typed.ast)

