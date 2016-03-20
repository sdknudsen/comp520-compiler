open Ast
open Parser
open Tokens
open AuxFunctions

(* let ppTable gamma outc =
  let str_of_typ = function
    | TInt -> "int"
    | TFloat -> "float"
    | TString -> "string" in
  Ctx.iter (fun k v -> Printf.fprintf outc "%s" (k ^ " : " ^ str_of_typ v ^ "\n")) gamma *)
let may f = function
  | Some typ -> f typ
  | None -> ()

(* remove unused ()s !! *)
let pTree (Prog(pkg,decls)) outc =
  let tabc = ref 0 in (* tab count *)
  let pln() = Printf.fprintf outc "\n" in (* print line *)
  let pstr s = Printf.fprintf outc "%s" s in (* print ocaml string *)
  let rec tabWith n = if n <= 0 then () else (pstr "\t"; tabWith (n-1)) in
  let tab() = tabWith !tabc in
  let pssl s f = (* print string separated list *)
    List.iter (fun y -> f y; pstr s)
  in
  let pcsl f = function (* print comma separated list *)
    | [] -> ()
    | x::xs -> f x; List.iter (fun y -> pstr ", "; f y) xs
  in
  (* let plns f = in *)
  let rec pTyp = function
    (* | Struct_type((typ_id, typ)s) ->  *)
    | TSimp(typ_id) -> pstr typ_id
    | TStruct(x_typ_ls) ->
       Printf.fprintf outc "struct {\n%t\n}"
                      (fun c -> pssl "; " (fun (x,typ) -> pstr (x^" "); pTyp typ) x_typ_ls)
    | TArray(typ,d) -> Printf.fprintf outc "[%d]%t" d (fun c -> pTyp typ)
    | TSlice(typ) -> Printf.fprintf outc "[]%t" (fun c -> pTyp typ)
    | Void -> ()
  in
  let rec pExpr = function
    (* | LValue(x) -> Printf.fprintf outc "%s" x *)
    | Lvalue(l) -> pLVal l
    | ILit(d) -> Printf.fprintf outc "%d" d
    | FLit(f) -> Printf.fprintf outc "%f" f
    | BLit(b) -> Printf.fprintf outc "%b" b
    | RLit(c) -> Printf.fprintf outc "'%c'" c
    | SLit(s) -> Printf.fprintf outc "\"%s\"" s
    | Bexp(op,e1,e2) -> Printf.fprintf outc "(%t %s %t)"
                                       (fun c -> pExpr e1)
                                       (bop_to_str op)
                                       (fun c -> pExpr e2)
    | Uexp(op,e) -> Printf.fprintf outc "(%s %t)"
                                   (uop_to_str op)
                                   (fun c -> pExpr e)
    | Fn_call(fun_id, es) -> Printf.fprintf outc "%t(%t)" (fun c -> pLVal fun_id) (fun c -> pcsl pExpr es)
    | Append(x, e) -> Printf.fprintf outc "append(%t,%t)"
                                     (fun c -> pstr x)
                                     (fun c -> pExpr e)

  and pLVal = function
    | Iden(id) -> pstr id
    | AValue(r,e) -> Printf.fprintf outc "%t[%t]" (fun c -> pLVal r) (fun c -> pExpr e)
    | SValue(r,id) -> Printf.fprintf outc "%t.%t" (fun c -> pLVal r) (fun c -> pstr id)

  in
  (* and pStmt = pBareStmt; pstr ";\n" *)
  let rec pStmt = function
    | Assign(xs, es) ->
       Printf.fprintf outc "%t = %t"
                      (fun c -> pcsl pLVal xs)
                      (fun c -> pcsl pExpr es)
    | Print(es) -> Printf.fprintf outc "print(%t)" (fun c -> pcsl pExpr es)
    | Println(es) -> Printf.fprintf outc "println(%t)" (fun c -> pcsl pExpr es)
    | If_stmt(po,e,ps,pso) ->
       Printf.fprintf outc "if %t%t {\n%t}%t"
                      (fun c -> incr tabc; may (fun p -> pStmt p; pstr "; ") po)
                      (fun c -> pExpr e)
                      (fun c -> pssl ";\n" (fun p -> tab(); pStmt p) ps)
                      (fun c -> (match pso with
                                   Some qs ->
                                     pstr " else {\n";
                                     pssl ";\n" (fun q -> tab(); pStmt q) qs;
                                     pstr "}"
                                 | None -> ()); decr tabc)
    | Block(stmts) ->
       Printf.fprintf outc "{\n%t"
                      (fun c -> incr tabc; List.iter pStmt stmts; decr tabc; pstr "}") (*default??*)
            

    | Switch_stmt(po, eo, ps) ->
       tab();
       Printf.fprintf outc "switch %t%t{\n%t"
                      (fun c -> incr tabc; may (fun p -> pStmt p; pstr "; ") po)
                      (fun c -> may (fun e -> pExpr e; pstr " ") eo)
                      (fun c -> List.iter pStmt ps; decr tabc; pstr "}"; decr tabc) (*default??*)
    | Switch_clause(eso, ps) ->
       tab();
       (match eso with
        | None ->
            Printf.fprintf outc "default:\n%t"
                           (fun c -> tab(); pssl ";\n" pStmt ps)
        | Some es -> 
            Printf.fprintf outc "case %t:\n%t"
                           (fun c -> tab(); pcsl pExpr es)
                           (fun c -> tab(); pssl ";\n" pStmt ps))

    | For_stmt(po1, eo, po2, ps) ->
       tab();
       Printf.fprintf outc "for %t; %t; %t {\n%t}"
                      (fun c -> may pStmt po1)
                      (fun c -> may pExpr eo)
                      (fun c -> may pStmt po2)
                      (fun c -> pssl ";\n" pStmt ps)

    | Var_stmt(ids_eso_typo_ls) ->
       pstr "var(\n"; incr tabc;
       List.iter (fun (ids,eso,typo) ->
           Printf.fprintf outc "%t %t%t;\n"
                          (* (fun c -> tab()) *)
                          (fun c -> pcsl pstr ids)
                          (fun c -> may pTyp typo)
                          (fun c -> may (fun es -> pstr " = "; pcsl pExpr es) eso)
         ) ids_eso_typo_ls; pstr ")"; decr tabc

    | SDecl_stmt(ids, eso) -> 
       Printf.fprintf outc "%t%t"
                      (fun c -> pcsl pstr ids)
                      (fun c -> may (fun es -> pstr " := "; pcsl pExpr es) eso)

    | Type_stmt(id_typ_ls) -> 
       List.iter (fun (id,typ) ->
           Printf.fprintf outc "type %t %t"
                          (fun c -> pstr id)
                          (fun c -> pTyp typ)
         ) id_typ_ls

    | Expr_stmt e -> pExpr e; pstr "" (* is this right ?? *)
    | Return(eo) -> Printf.fprintf outc "return %t"
                                   (fun c -> may (fun e -> pstr " "; pExpr e) eo)
    | Break -> Printf.fprintf outc "break"
    | Continue -> Printf.fprintf outc "continue"
    | Empty_stmt -> ()

                 (* | Var_stmt((ids, eso, typo)s)   *)
                 (* | SDecl_stmt((ids, eso))   *)
                 (* | Type_stmt((id, typ)s)   *)
  in
  let rec pDecl =
    tab(); function
    | Var_decl(ids_eso_typo_ls) ->
       pstr "var(\n"; incr tabc;
       List.iter (fun (ids,eso,typo) ->
           Printf.fprintf outc "%t %t%t;\n"
                          (* (fun c -> tab()) *)
                          (fun c -> pcsl pstr ids)
                          (fun c -> may pTyp typo)
                          (fun c -> may (fun es -> pstr " = "; pcsl pExpr es) eso)
         ) ids_eso_typo_ls; pstr ")\n"; decr tabc
    (* may (fun typ -> ppStr (string_of_typ typ)) typo; *)
    (* may (fun es -> (ppStr " = "); pcsl es) eso; ppStr ";" *)
    | Type_decl(typId_typ_ls) -> 
       pstr "type(\n"; incr tabc;
       List.iter (fun (id,typ) ->
           Printf.fprintf outc "%t %t;\n"
                          (fun c -> pstr id)
                          (fun c -> pTyp typ)
         ) typId_typ_ls; pstr ")\n"; decr tabc

    | Func_decl(fId, id_typ_ls, typ, ps) -> 
       Printf.fprintf outc "func %t(%t) %t {\n%t}\n"
                      (fun c -> pstr fId)
                      (fun c -> pcsl (fun (id,typ) -> pstr (id^" "); pTyp typ) id_typ_ls)
                      (fun c -> pTyp typ)
                      (* (fun c -> List.iter (fun x -> ()) ps); *)
                      (* change this !! *)
                      (fun c -> pssl ";\n" pStmt ps)
  in
  pstr ("package "^pkg); pln(); pln(); List.iter pDecl decls

