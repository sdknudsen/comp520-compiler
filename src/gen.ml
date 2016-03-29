open Ast
open Context
open Ho 
open AuxFunctions

let generate (Prog(id,decls) : Typed.ast) =
  let rec pTyp (at:Typed.uttyp) = match at with
    | TSimp(typ_id) -> ()
    | TStruct(x_typ_ls) -> ()
    | TArray(typ,d) -> ()
    | TSlice(typ) -> ()
    | TVoid -> ()
    | TFn(a,b) -> ()
    | TKind(a) -> ()
  in
  let rec pExpr ((ue,(pos,typ)):Typed.annotated_texpr) =
    | Iden(id) -> ()
    | AValue(r,e) -> ()
    | SValue(r,id) -> ()
    | Parens(e)  -> ()
    | ILit(d) -> ()
    | FLit(f) -> ()
    | RLit(c) -> ()
    | SLit(s) -> ()
    | Bexp(op,e1,e2) -> ()
    | Uexp(op,e) -> ()
    | Fn_call((Iden(i),_), k) -> ()
    | Fn_call(fun_id, es) -> ()
    | Append(x, e) -> ()
  in
  let rec pStmt ((us, pos): Typed.annotated_utstmt) = match us with
    | Assign(xs, es) -> ()
    | Var_stmt(xss) -> ()
    | Print(es) -> ()
    | Println(es) -> ()
    | If_stmt(po,e,ps,pso) -> ()
    | Block(stmts) -> ()
    | Switch_stmt(po, eo, ps) -> ()
    | Switch_clause(eso, ps) -> ()
    | For_stmt(po1, eo, po2, ps) -> ()
    | SDecl_stmt(id_e_ls) -> ()
    | Type_stmt(id_typ_ls) -> ()
    | Expr_stmt e -> ()
    | Return(eo) -> ()
    | Break -> ()
    | Continue -> ()
    | Empty_stmt -> ()
  in
  let rec pDecl ((ud,pos): Typed.annotated_utdecl) = 
           | Var_decl(xss) -> ()
           | Type_decl(id_atyp_ls) -> ()
           | Func_decl(fId, id_typ_ls, typ, ps) -> ()
  in
  pstr ("package "^id); pln(); pln(); pssl "\n" pDecl decls

