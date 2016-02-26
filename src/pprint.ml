open Ast
open Parser
open Tokens

(* let ppTable gamma outc =
  let str_of_typ = function
    | TInt -> "int"
    | TFloat -> "float"
    | TString -> "string" in
  Ctx.iter (fun k v -> Printf.fprintf outc "%s" (k ^ " : " ^ str_of_typ v ^ "\n")) gamma *)

let uop_to_str = function
    Positive -> "+"           
  | Negative -> "-"
  | Boolnot -> "!"
  | Bitnot -> "^"

let bop_to_str = function
    Boolor -> "||"
  | Booland -> "&&"
  | Equals -> "=="
  | Notequals -> "!="
  | Lt -> "<"
  | Lteq -> "<="
  | Gt -> ">"
  | Gteq -> ">="
  | Plus -> "+"
  | Minus -> "-"
  | Bitor -> "|"
  | Bitand -> "&"
  | Bitnand -> "&^"
  | Bitxor -> "^"
  | Times -> "*"
  | Div -> "/"
  | Modulo -> "%"
  | Lshift -> "<<"
  | Rshift -> ">>"

let may f = function
  | Some typ -> f typ
  | None -> ()
              
(* let ppTree (Prog(pkg,stmts)) outc = *)
(*   let tabCount = ref 0 in *)
(*   let println() = Printf.fprintf outc "\n" in *)
(*   let ppStr s = Printf.fprintf outc "%s" s in *)
(*   let rec tabWith n = if n <= 0 then () else (ppStr "\t"; tabWith (n-1)) in *)
(*   let tab() = tabWith !tabCount in *)
(*   let pcsl = function (\* print comma separated list *\) *)
(*       [] -> () *)
(*     | x::xs -> ppStr x; List.iter (fun y -> ppStr y) xs *)
(*   in *)
(*   let rec ppExpr = function *)
(*     | ILit(d) -> Printf.fprintf outc "%d" d *)
(*     | FLit(f) -> Printf.fprintf outc "%f" f *)
(*     | BLit(b) -> Printf.fprintf outc "%b" b *)
(*     | RLit(c) -> Printf.fprintf outc "'%c'" c *)
(*     | SLit(s) -> Printf.fprintf outc "\"%s\"" s *)
(*     | Iden(x) -> Printf.fprintf outc "%s" x *)
(*     | Bexp(op,e1,e2) -> ppStr "("; ppExpr e1; ppStr (bop_to_str op); ppExpr e2; ppStr ")" *)
(*     | Uexp(op,e) -> ppStr "("; ppStr (uop_to_str op); ppExpr e; ppStr ")" *)

(*   and printDecl = function *)
(*       Var_decl(xs, eso, typo) -> *)
(*        ppStr "var "; pcsl xs;  *)
(*        may (fun typ -> ppStr (string_of_typ typ)) typo;  *)
(*        may (fun es -> (ppStr " = "); pcsl es) eso; ppStr ";" *)

(*     | Slice_decl(xs, typ) -> *)
(*        ppStr "var "; pcsl xs;  *)
(*        ppStr ("[]"^(string_of_typ typ)); ppStr ";" *)

(*     | Array_decl(xs, d, typ) -> *)
(*        ppStr "var "; pcsl xs;  *)
(*        ppStr (" [" ^ string_of_int d ^ "]"^(string_of_typ typ) ^ ";") *)

(*     | Type_decl(x, typ) -> *)
(*        ppStr "type "; ppStr x; ppStr " "; ppStr typ; ppStr ";" *)

(*     | Struct_decl(x, ss) -> *)
(*        Printf.fprintf outc "%ttype %t struct {\n%t\n"  *)
(*                       (fun c -> tab()) *)
(*                       (fun c -> ppStr x; incr tabCount) *)
(*                       (fun c -> List.iter (fun (vars,typ) -> tab(); ppStr "var "; pcsl vars; ppStr typ; ppStr ";") ss; decr tabCount) *)
    
(*     | Func_decl(f, xstLs, ss, yo, typo) -> (\* ends with 0 *\) *)
(*        ppStr "func "^f^"("; *)
(*        (match xstLs with *)
(*         | [] -> () *)
(*         | (xs,t)::tl -> *)
(*            pcsl xs; ppStr (string_of_typ t); *)
(*            List.iter (fun (xs,t) -> *)
(*                ppStr ", "; pcsl xs; ppStr (" "^(string_of_typ t))) tl); *)
(*        ppStr ") "; *)
(*        may (fun y -> ppStr (y ^ " ")) yo;  *)
(*        may (fun typ -> ppStr (string_of_typ typ) ^ " ") typo;  *)
(*        ppStr "{\n"; *)
(*        ppStmts ss; *)
(*        ppStr "return"; *)
(*        may (fun y -> ppStr " "^y) yo;  *)
(*        ppStr "\n}"; *)

(*     | Empty -> () *)

(*   and ppStmt = function *)
(*       Assign(x) -> tab(); ppStr (x ^ " = "); ppExpr e; ppStr ";\n" *)
(*       (\* of 'e assignment *\) *)
(*     | Print(x) -> tab(); ppStr "print "; ppExpr e; ppStr ";\n" (\* add println ! *\) *)
(*       (\* of 'e *\) *)
(*     | If_stmt(e,xs,yso) ->  *)
(*        Printf.fprintf outc "%tif %t {\n%t}\n%t" *)
(*                       (fun c -> tab()) *)
(*                       (fun c ->  ppExpr e) *)
(*                       (fun c -> incr tabCount; List.iter ppStmt xs; may (fun ys -> tabWith(!tabCount-1); ppStr "else\n"; List.iter ppStmt ys) yso)  *)
(*                       (fun c -> decr tabCount) *)

(*     | For_stmt(eo,ss) -> *)
(*        Printf.fprintf outc "%tfor %t {\n%t}\n%t" *)
(*                       (fun c -> tab()) *)
(*                       (fun c -> may (fun e -> ppExpr e) eo) *)
(*                       (fun c -> incr tabCount; List.iter ppStmt ss)  *)
(*                       (fun c -> decr tabCount) *)

(*     | Var_stmt(xs,eso,typo) -> *)
(*        Printf.fprintf outc "%tvar %t;\n" *)
(*                       (fun c -> tab()) *)
(*                       (fun c -> pcsl xs; *)
(*                                 may (fun typ -> ppStr (" "^string_of_typ typ)) eso; *)
(*                                 may (fun es -> ppStr " = "; pcsl es) eso) *)

(*     | Slice_stmt(xs,d,typ) ->  *)
(*        Printf.fprintf outc "var %t[]%t\n"  *)
(*                       (fun c -> tab(); pcsl xs) *)
(*                       (fun c -> ppStr (string_of_typ typ)) *)
     
(*     | Array_stmt(xs,d,typ) ->  *)
(*        Printf.fprintf outc "%t[%t] %t\n"  *)
(*                       (fun c -> tab(); ppStr xs) *)
(*                       (fun c -> ppStr (string_of_int d)) *)
(*                       (fun c -> ppStr v) *)

(*     | Type_stmt(x, typ) -> *)
(*        ppStr "type "; ppStr x; ppStr " "; ppStr typ; ppStr ";" *)

(*     | Struct_stmt(x, ss) -> *)
(*        Printf.fprintf outc "%ttype %t struct {\n%t\n"  *)
(*                       (fun c -> tab()) *)
(*                       (fun c -> ppStr x; incr tabCount) *)
(*                       (fun c -> List.iter (fun (vars,typ) -> tab(); ppStr "var "; pcsl vars; ppStr typ; ppStr ";") ss; decr tabCount) *)
    

(*     | Empty -> () *)

(*                  (\* missing incr and decr? *\) *)

(*   in *)
(*   Printf.fprintf outc "%t\n%t"  *)
(*                  (fun c -> ppString pkg) *)
(*                  (fun c -> List.iter ppStmt stmts) *)





       (* let rec pfdecls = function *)
       (*   | [] -> () *)
       (*   | [(xs,t)] -> pcsl xs; ppStr (" "^(string_of_typ t)) *)
       (*   | (xs,t)::tl -> pcsl xs; ppStr (" "^(string_of_typ t)^", "); pfdecls tl *)
       (* in *)

       (* List.iter (fun (xs,t) -> pcsl xs; ppStr (" "^(string_of_typ t)^", ")) xstLs; *)

  (* let pcsl ls =  *)
  (*   List.iteri (fun i x -> if i > 0 then ppStr (" ," ^ x) else ppStr x) ls;; *)

(* type declaration = Dec of id * id *)

              
(* remove unused ()s !! *)
let pTree (Prog(pkg,decls)) outc =
  let tabc = ref 0 in (* tab count *)
  let pln() = Printf.fprintf outc "\n" in (* print line *)
  let pstr s = Printf.fprintf outc "%s" s in (* print ocaml string *)
  let rec tabWith n = if n <= 0 then () else (pstr "\t"; tabWith (n-1)) in
  let tab() = tabWith !tabc in
  let pcsl f = function (* print comma separated list *)
    | [] -> ()
    | x::xs -> f x; List.iter (fun y -> pstr ", "; f y) xs
  in
  let rec pTyp = function
  (* | Struct_type((typ_id, typ)s) ->  *)
  | Simple_type(typ_id) -> pstr typ_id
  | Struct_type(xtyps) -> ()
  | Array_type(typ) -> ()
  | Slice_type(typ) -> ()
  | Void -> ()
  in
  let rec pExpr = function
    (* | AValue(x,y)  *)
    (* | SValue(x,y) -> () *)
    (* | LValue(x) -> Printf.fprintf outc "%s" x *)
    | Lvalue(l) -> ()
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
    | Fn_call(fun_id, es) -> ()
    | Append(x, e) -> ()

  and pLVal = function
  | Iden(id) -> ()
  | AValue(x, e) -> ()
  | SValue(x, id) -> ()

  in
  let rec pStmt =
    (* tab(); *)
    function
    | Assign(xs, es) ->
       Printf.fprintf outc "%t = %t;"
                      (fun c -> pcsl pstr xs)
                      (fun c -> pcsl pExpr es)
    | Print(es) -> Printf.fprintf outc "print %t;" (fun c -> pcsl pExpr es)
    | Println(es) -> Printf.fprintf outc "println %t;" (fun c -> pcsl pExpr es)
    | If_stmt(po,e,ps,pso) -> tab();
       Printf.fprintf outc "if %t%t {\n%t}\n%t"
                      (fun c -> incr tabc; may (fun p -> pStmt p; pstr "; ") po)
                      (fun c -> pExpr e)
                      (fun c -> List.iter pStmt ps)
                      (fun c -> may (fun ps -> pcsl pStmt ps) pso; decr tabc)
    | Switch_stmt(po, eo, ps) -> tab();
       Printf.fprintf outc "switch %t%t{\n%t"
                      (fun c -> incr tabc; may (fun p -> pStmt p; pstr "; ") po)
                      (fun c -> may (fun e -> pExpr e; pstr " ") eo)
                      (fun c -> List.iter pStmt ps; decr tabc; pstr "}\n") (*default??*)

    | Switch_clause(eso, ps) -> tab();
       Printf.fprintf outc "case %t: %t\n"
                      (fun c -> match eso with
                                | None -> pstr "default"
                                | Some es -> pcsl pExpr es)
                      (fun c -> pcsl pStmt ps)

    | For_stmt(p1o, eo, p2o, ps) -> tab()

    | Var_stmt(ids_eso_typo_ls) -> ()
    | SDecl_stmt(ids, eso) -> ()
    | Type_stmt(id_typ_ls) -> ()
    | Return(eo) -> Printf.fprintf outc "return%t;"
                                   (fun c -> may (fun e -> pstr " "; pExpr e) eo)
    | Break -> Printf.fprintf outc "break;"
    | Continue -> Printf.fprintf outc "continue;"
    | Empty -> ()

  (* | Var_stmt((ids, eso, typo)s)   *)
  (* | SDecl_stmt((ids, eso))   *)
  (* | Type_stmt((id, typ)s)   *)

  in
  let rec pDecl = tab(); function
    | Var_decl(ids_eso_typo_ls) -> pstr "var(\n";
       List.iter (fun (ids,eso,typo) ->
           Printf.fprintf outc "%t %t %t;\n"
                          (* (fun c -> tab()) *)
                          (fun c -> pcsl pstr ids)
                          (fun c -> may pTyp typo)
                          (fun c -> may (fun es -> pstr "= "; pcsl pExpr es) eso)
         ) ids_eso_typo_ls; pstr ")\n"

    (* may (fun typ -> ppStr (string_of_typ typ)) typo; *)
    (* may (fun es -> (ppStr " = "); pcsl es) eso; ppStr ";" *)
    | Type_decl(typId_typ_ls) -> ()
    | Func_decl(fId, id_typ_ls, typ, ps) -> ()
                                              (* | Func_decl(fId (id, typ)s, typ, ps) -> () *)

                                              (*   and printDecl = function *)
                                              (*       Var_decl(xs, eso, typo) -> *)
  in
  pstr ("package "^pkg); pln(); List.iter pDecl decls
