open Ast
open Parser
open Tokens
open AuxFunctions
open Ho
open List
open Context

let simplify (Prog(id,decls) : Typed.ast) =
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
        | None -> failwith "Not valid type type")
    (* | t -> Some(t) *)
    | TStruct(x_typ_ls) -> TStruct(map (fun(id,typ) -> id, sTyp typ) x_typ_ls)
    | TArray(typ,d) -> TArray(sTyp typ,d)
    | TSlice(typ) -> TSlice(sTyp typ)
    | TVoid -> TVoid
    | TFn(a,b) -> TFn(map sTyp a,sTyp b)
    | TKind(a) -> sTyp a
  in
  
  let rec sExpr ((ue,(pos,typ,d)):Typed.annotated_texpr) =
    let sue = match ue with
    | Iden(id) -> Iden(id)
    | AValue(r,e) -> AValue(sExpr r,sExpr e)
    | SValue(r,id) -> SValue(sExpr r,id)
    (* | Fn_call((Iden(_),(_, TKind(TSimp("#",_)), _)), _) *)
    | ILit(_) 
    | FLit(_)
    | RLit(_)
    | SLit(_) ->  ue
    | Bexp(op,e1,e2) -> Bexp(op,sExpr e1,sExpr e2)
    | Uexp(op,e) -> Uexp(op,sExpr e)
    | Fn_call((Iden(i),(a, TKind(typ), g)), es) -> 
         (match sTyp (sure (get_type_instance i g)) with
         | TSimp(baseName,ctx) -> Fn_call((Iden(baseName),(a, sure (get_type_instance "#" ctx), ctx)), map sExpr es) 
         | _ -> failwith "no")
    (* | Fn_call((Iden(i),a), k) -> failwith "no" *)
    | Fn_call(fun_id,es) -> Fn_call(fun_id,map sExpr es)
(* math type if type of iden is kind replace with base type *)

    | Append(x,e) -> Append(x,sExpr e)
    in (sue,(pos, sTyp typ ,d))
  in

  let rec sStmt ((us, pos): Typed.annotated_utstmt) =
    let sus = 
      match us with
      | Assign(xs, es) -> Assign(map sExpr xs,map sExpr es)
      | Var_stmt(xss) -> Var_stmt(map (map (fun (id,e,t) -> (id,mapo sExpr e,mapo sTyp t))) xss)

      | Print(es) -> Print(map sExpr es)
      | Println(es) -> Println(map sExpr es)
      | If_stmt(po,e,ps,pso) -> If_stmt(mapo sStmt po,sExpr e,map sStmt ps, mapo (map sStmt) pso)
      | Block(stmts) -> Block(map sStmt stmts)
      | Switch_stmt(po, eo, ps) -> Switch_stmt(mapo sStmt po, mapo sExpr eo, map sStmt ps)
      | Switch_clause(eso, ps) -> Switch_clause(mapo (map sExpr) eso, map sStmt ps)
(* map (fun (id,e) -> (id,sExpr e))  *)

      | For_stmt(po1, eo, po2, ps) -> For_stmt(mapo sStmt po1, mapo sExpr eo, mapo sStmt po2, map sStmt ps)
      | SDecl_stmt(id_e_ls) -> SDecl_stmt(map (fun (id,e) -> (id,sExpr e)) id_e_ls)
      | Type_stmt(id_typ_ls) -> Type_stmt(map (fun (id,t) -> (id,sTyp t)) id_typ_ls)
      | Expr_stmt e -> Expr_stmt(sExpr e)
      | Return(eo) -> Return(mapo sExpr eo)
      | Break -> Break
      | Continue -> Continue
      | Empty_stmt -> Empty_stmt
    in (sus,pos)
  in

  let rec sDecl ((ud,pos): Typed.annotated_utdecl) = 
    let sud = match ud with
      | Var_decl(xss) ->
         Var_decl(map (map (fun (id,e,t) -> (id,mapo sExpr e,mapo sTyp t))) xss)
      | Type_decl(id_atyp_ls) ->
         Type_decl(map (fun(id,t) -> (id,sTyp t)) id_atyp_ls)
      | Func_decl(fId, id_typ_ls, typ, ps) ->
         Func_decl(fId, map (fun(id,t) -> (id,sTyp t)) id_typ_ls, sTyp typ, map sStmt ps)
    in (sud,pos)
  in
  (Prog(id, map sDecl decls) : Typed.ast)

    (* I don't change ids *)
