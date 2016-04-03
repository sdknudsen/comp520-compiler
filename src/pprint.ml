open Ast
open Parser
open Tokens
open AuxFunctions
       (* for typed tree: *)

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
(* let pTree (Prog(pkg,decls) : Untyped.ast) outc = *)
let pTree (Prog(id,decls) : Untyped.ast) outc =
  let tabc = ref 0 in (* tab count *)
  let pln() = Printf.fprintf outc "\n" in (* print line *)
  let pstr s = Printf.fprintf outc "%s" s in (* print ocaml string *)
  let pid id = pstr (fst id) in
  let rec tabWith n = if n <= 0 then () else (pstr "\t"; tabWith (n-1)) in
  let tab() = tabWith !tabc in
  let pssl s f = (* print string separated list *)
    List.iter (fun y -> f y; pstr s)
  in
  let pcsl f = function (* print comma separated list *)
    | [] -> ()
    | x::xs -> f x; List.iter (fun y -> pstr ", "; f y) xs
  in
  let plsl f = function (* print comma separated list *)
    | [] -> ()
    | x::xs -> f x; List.iter (fun y -> pstr "\n"; f y) xs
  in
  (* let plns f = in *)
  let rec pTyp (at:Untyped.uttyp) = match at with
    (* | Struct_type((typ_id, typ)s) ->  *)
    | TSimp(typ_id) -> pstr (fst typ_id) (* snd is lexing positon *)
    | TStruct(x_typ_ls) ->
       Printf.fprintf outc "struct {\n%t\n}"
                      (fun c -> pssl "; " (fun (x,typ) -> pstr (fst x^" "); pTyp typ) x_typ_ls)
    | TArray(typ,d) -> Printf.fprintf outc "[%d]%t" d (fun c -> pTyp typ)
    | TSlice(typ) -> Printf.fprintf outc "[]%t" (fun c -> pTyp typ)
    | TVoid -> ()
                 (*add this to typed print Printf.fprintf outc "#Void" *)
    | TFn(a,b) -> 
       Printf.fprintf outc "(%t) %t"
                      (fun c -> pcsl pTyp a)
                      (fun c -> pTyp b)
                      
    | TKind(a) -> Printf.fprintf outc "#%t" (fun c -> pTyp a)

  in
  (* let rec pExpr = function *)
  let rec pExpr ((ue,pos):Untyped.annotated_utexpr) = match ue with
    | Iden(id) -> pstr (fst id)
    | AValue(r,e) -> Printf.fprintf outc "%t[%t]" (fun c -> pExpr r) (fun c -> pExpr e)
    | SValue(r,id) -> Printf.fprintf outc "%t.%t" (fun c -> pExpr r) (fun c -> pstr (fst id))
    (* | Parens(e)  -> Printf.fprintf outc "(%t)"
                                   (fun c -> pExpr e) *)
    | ILit(d) -> Printf.fprintf outc "%d" d
    | FLit(f) -> Printf.fprintf outc "%f" f
    (* | BLit(b) -> Printf.fprintf outc "%b" b *)
    | RLit(c) -> Printf.fprintf outc "'%c'" c
    | SLit(s) -> Printf.fprintf outc "\"%s\"" s
    | Bexp(op,e1,e2) -> Printf.fprintf outc "(%t %s %t)"
                                       (fun c -> pExpr e1)
                                       (bop_to_str op)
                                       (fun c -> pExpr e2)
    | Uexp(op,e) -> Printf.fprintf outc "(%s %t)"
                                   (uop_to_str op)
                                   (fun c -> pExpr e)
    | Fn_call(fun_id, es) -> Printf.fprintf outc "%t(%t)" (fun c -> pExpr fun_id) (fun c -> pcsl pExpr es)
    | Append(x, e) -> Printf.fprintf outc "append(%t,%t)"
                                     (fun c -> pstr (fst x)) (* check that we don't need the annotation (snd x) *)
                                     (fun c -> pExpr e)


  in
  let rec pStmt ((us, pos): Untyped.annotated_utstmt) = match us with
    | Assign(xs, es) ->
       Printf.fprintf outc "%t = %t"
                      (fun c -> pcsl pExpr xs)
                      (fun c -> pcsl pExpr es)
    | Var_stmt(xss) ->
       plsl (fun xs ->
           pstr "var(\n"; incr tabc;
           List.iter (fun ((s,_),eso,typo,_) ->
               Printf.fprintf outc "%t %t%t;\n"
                              (fun c -> pstr s)
                              (fun c -> may (fun t -> pstr " "; pExpr t) eso)
                              (fun c -> may (fun t -> pstr " = "; pTyp t) typo)
             ) xs; pstr ")"; decr tabc) xss
       (* plsl (fun xs -> *)
       (*     Printf.fprintf outc "var (%t)" *)
       (*                    (fun c -> List.iter *)
       (*                                (fun ((s,_),ut,typ) -> *)
       (*                                  Printf.fprintf outc "%t%t%t" *)
       (*                                                 (fun c -> pstr s) *)
       (*                                                 (fun c -> match ut with *)
       (*                                                           | None -> () *)
       (*                                                           | Some u -> pstr " "; pExpr u) *)
       (*                                                 (fun c -> match typ with *)
       (*                                                           | None -> () *)
       (*                                                           | Some ty -> pstr " = "; pTyp ty) *)
       (*                                ) xs)) xss *)
    (* Printf.fprintf outc "print(%t)" (fun c -> pcsl pStmt stmts) *)
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

    | SDecl_stmt(id_e_ls) ->
       let (ids,es) = unzip id_e_ls in
       Printf.fprintf outc "%t%t"
                      (fun c -> pcsl pid ids)
                      (fun c -> pstr " := "; pcsl pExpr es)

    | Type_stmt(id_typ_ls) ->
       List.iter (fun (id,typ) ->
           Printf.fprintf outc "type %t %t"
                          (fun c -> pid id)
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
  let rec pDecl ((ud,pos): Untyped.annotated_utdecl) = 
    tab(); match ud with
           | Var_decl(xss) -> 
       plsl (fun xs ->
           pstr "var(\n"; incr tabc;
           List.iter (fun ((s,_),eso,typo) ->
               Printf.fprintf outc "%t %t%t;\n"
                              (fun c -> pstr s)
                              (fun c -> may (fun t -> pstr " "; pExpr t) eso)
                              (fun c -> may (fun t -> pstr " = "; pTyp t) typo)
             ) xs; pstr ")"; decr tabc) xss
              (* List.iter (fun this -> *)
              (* pstr "var(\n"; incr tabc; *)
              (* List.iter (fun (id_pos_ls,eso,typo) -> *)
              (*     let ids = List.map (fun (x,y) -> x) id_pos_ls in *)
              (*     (\* let typo = mapo (fun (x,y) -> x) typo_pos in *\) *)
              (*     Printf.fprintf outc "%t %t%t;\n" *)
              (*                    (\* (fun c -> tab()) *\) *)
              (*                    (fun c -> pcsl pstr ids) *)
              (*                    (fun c -> may pTyp typo) *)
              (*                    (fun c -> may (fun es -> pstr " = "; pcsl pExpr es) eso) *)
              (*   ) this; pstr ")\n"; decr tabc *)
              (*   ) ids_eso_typo_ls *)

           | Type_decl(id_atyp_ls) -> 
              pstr "type(\n"; incr tabc;
              List.iter (fun (id,atyp) ->
                  Printf.fprintf outc "%t %t;\n"
                                 (fun c -> pid id)
                                 (fun c -> pTyp atyp)
                ) id_atyp_ls; pstr ")\n"; decr tabc

           | Func_decl(fId, id_typ_ls, typ, ps) -> 
              Printf.fprintf outc "func %t(%t) %t {\n%t}\n"
                             (fun c -> pid fId)
                             (* not sure using fst is the right thing to do: *)
                             (fun c -> pcsl (fun (id,typ) -> pstr (fst id^" "); pTyp typ) id_typ_ls)
                             (fun c -> pTyp typ)
                             (* (fun c -> List.iter (fun x -> ()) ps); *)
                             (* change this !! *)
                             (fun c -> pssl ";\n" pStmt ps)
  in
  pstr ("package "^(fst id)); pln(); pln(); List.iter pDecl decls


(********************************************************************************)

let ptTree (Prog(id,decls) : Typed.ast) outc =
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
  let plsl f = function (* print comma separated list *)
    | [] -> ()
    | x::xs -> f x; List.iter (fun y -> pstr "\n"; f y) xs
  in
  (* let plns f = in *)
  let rec pTyp (at:Typed.uttyp) = match at with
    (* | Struct_type((typ_id, typ)s) ->  *)
    | TSimp(typ_id) -> pstr (fst typ_id)
    | TStruct(x_typ_ls) ->
       Printf.fprintf outc "struct {\n%t\n}"
                      (fun c -> pssl "; " (fun (x,typ) -> pstr (x^" "); pTyp typ) x_typ_ls)
    | TArray(typ,d) -> Printf.fprintf outc "[%d]%t" d (fun c -> pTyp typ)
    | TSlice(typ) -> Printf.fprintf outc "[]%t" (fun c -> pTyp typ)
    | TVoid -> ()
                 (*add this to typed print Printf.fprintf outc "#Void" *)
    | TFn(a,b) -> 
       Printf.fprintf outc "(%t) %t"
                      (fun c -> pcsl pTyp a)
                      (fun c -> pTyp b)
                      
    | TKind(a) -> Printf.fprintf outc "#%t" (fun c -> pTyp a)

  in
  (* let rec pExpr = function *)
  let rec pExpr ((ue,(pos,typ)):Typed.annotated_texpr) =
    Printf.fprintf outc "%t /*:%t*/"
    (fun c -> match ue with
    | Iden(id) -> pstr id
    | AValue(r,e) -> Printf.fprintf outc "%t[%t]" (fun c -> pExpr r) (fun c -> pExpr e)
    | SValue(r,id) -> Printf.fprintf outc "%t.%t" (fun c -> pExpr r) (fun c -> pstr id)
    (* | Parens(e)  -> Printf.fprintf outc "(%t)"
                                   (fun c -> pExpr e) *)
    | ILit(d) -> Printf.fprintf outc "%d" d
    | FLit(f) -> Printf.fprintf outc "%f" f
    (* | BLit(b) -> Printf.fprintf outc "%b" b *)
    | RLit(c) -> Printf.fprintf outc "'%c'" c
    | SLit(s) -> Printf.fprintf outc "\"%s\"" s
    | Bexp(op,e1,e2) -> Printf.fprintf outc "(%t %s %t)"
                                       (fun c -> pExpr e1)
                                       (bop_to_str op)
                                       (fun c -> pExpr e2)
    | Uexp(op,e) -> Printf.fprintf outc "(%s %t)"
                                   (uop_to_str op)
                                   (fun c -> pExpr e)
    | Fn_call((Iden(i),_), k) -> Printf.fprintf outc "%t(%t)"
                                                (fun c -> pstr i)
                                                (fun c -> pcsl pExpr k)
    | Fn_call(fun_id, es) -> Printf.fprintf outc "%t(%t)" (fun c -> pExpr fun_id) (fun c -> pcsl pExpr es)
    | Append(x, e) -> Printf.fprintf outc "append(%t,%t)"
                                     (fun c -> pstr (x)) 
                                     (fun c -> pExpr e)
    )
    (fun c -> pTyp typ)

  in
  let rec pStmt ((us, pos): Typed.annotated_utstmt) = match us with
    | Assign(xs, es) ->
       Printf.fprintf outc "%t = %t"
                      (fun c -> pcsl pExpr xs)
                      (fun c -> pcsl pExpr es)
    | Var_stmt(xss) ->
       plsl (fun xs ->
           pstr "var(\n"; incr tabc;
           List.iter (fun (s,eso,typo,_) ->
               Printf.fprintf outc "%t %t%t;\n"
                              (fun c -> pstr s)
                              (fun c -> may (fun t -> pstr " "; pExpr t) eso)
                              (fun c -> may (fun t -> pstr " = "; pTyp t) typo)
             ) xs; pstr ")"; decr tabc) xss

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

    | SDecl_stmt(id_e_ls) ->
       let (ids,es) = unzip id_e_ls in
       Printf.fprintf outc "%t%t"
                      (fun c -> pcsl pstr ids)
                      (fun c -> pstr " := "; pcsl pExpr es)

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
  in
  let rec pDecl ((ud,pos): Typed.annotated_utdecl) = 
    tab(); match ud with
           | Var_decl(xss) -> 
             plsl (fun xs ->
             pstr "var(\n"; incr tabc;
             List.iter (fun (s,eso,typo) ->
                 Printf.fprintf outc "%t %t%t;\n"
                              (fun c -> pstr s)
                              (fun c -> may (fun t -> pstr " "; pTyp t) typo)
                              (fun c -> may (fun t -> pstr " = "; pExpr t) eso)
             ) xs; pstr ")"; decr tabc) xss

           | Type_decl(id_atyp_ls) -> 
              pstr "type(\n"; incr tabc;
              List.iter (fun (id,atyp) ->
                  Printf.fprintf outc "%t %t;\n"
                                 (fun c -> pstr id)
                                 (fun c -> pTyp atyp)
                ) id_atyp_ls; pstr ")\n"; decr tabc

           | Func_decl(fId, id_typ_ls, typ, ps) -> 
              Printf.fprintf outc "func %t(%t) %t {\n%t}"
                             (fun c -> pstr fId)
                             (* not sure using fst is the right thing to do: *)
                             (fun c -> pcsl (fun (id,typ) -> pstr (id^" "); pTyp typ) id_typ_ls)
                             (fun c -> pTyp typ)
                             (* (fun c -> List.iter (fun x -> ()) ps); *)
                             (* change this !! *)
                             (fun c -> pssl ";\n" pStmt ps)
  in
  pstr ("package "^id); pln(); pln(); pssl "\n" pDecl decls

