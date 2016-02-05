open Ast
open CFunctions
       
(* let ppTable gamma outc =
  let str_of_typ = function
    | TInt -> "int"
    | TFloat -> "float"
    | TString -> "string" in
  Ctx.iter (fun k v -> Printf.fprintf outc "%s" (k ^ " : " ^ str_of_typ v ^ "\n")) gamma *)

let ppTree (TProg(decls,stmts)) outc =
  let tabCount = ref 0 in
  let println() = Printf.fprintf outc "\n" in
  let ppStr s = Printf.fprintf outc "%s" s in
  let rec tabWith n = if n <= 0 then () else (ppStr "\t"; tabWith (n-1)) in
  let tab() = tabWith !tabCount in
  let rec ppBinExp e1 e2 op = ppStr "("; ppExpr e1; ppStr op; ppExpr e2; ppStr ")"
  and ppExpr te =
    match fst te with
    | ILit(v) -> Printf.fprintf outc "%d" v
    | FLit(v) -> Printf.fprintf outc "%f" v
    | SLit(v) -> ppStr ("\"" ^ v ^ "\"")
    | Var(x) -> ppStr x
    | Bexp(op,e1,e2) -> ppBinExp e1 e2 (str_of_binop op)
    | Uexp(op,e) -> ppStr ((str_of_unop op)^"("); ppExpr e; ppStr ")"
  in
  let printDecl (Dec(id,typ)) = match typ with
    | TInt -> ppStr ("var " ^ id ^ " : int;\n")
    | TString -> ppStr ("var " ^ id ^ " : string;\n")
    | TFloat -> ppStr ("var " ^ id ^ " : float;\n")
  in
  let rec ppStmt = function
    | Assign(id,e) -> tab(); ppStr (id ^ " = "); ppExpr e; ppStr ";\n"
    | Print(e) -> tab(); ppStr "print "; ppExpr e; ppStr ";\n"
    | Read(id) -> tab(); ppStr "read "; ppStr id; ppStr ";\n"

    | Ifte(e,xs,ys) ->
       tab(); ppStr "if "; ppExpr e; ppStr " then\n"; incr tabCount;
       List.iter ppStmt xs; tabWith(!tabCount-1); ppStr "else\n";
       List.iter ppStmt ys; decr tabCount;
       tab(); ppStr "endif\n"

    | Ift(e,xs) ->
       tab(); ppStr "if "; ppExpr e; ppStr " then\n"; incr tabCount;
       List.iter ppStmt xs; decr tabCount; tab(); ppStr "endif\n"

    | While(e,xs) ->
       tab(); ppStr "while "; ppExpr e; ppStr " do\n"; incr tabCount;
       List.iter ppStmt xs; decr tabCount; tab(); ppStr "done\n"
  in
  List.iter printDecl decls; println(); List.iter ppStmt stmts

(* let ppC ((TProg(decls,stmts)),gamma) outc =
  let tabCount = ref 1 in
  let println() = Printf.fprintf outc "\n" in
  let ppStr s = Printf.fprintf outc "%s" s in
  let rec tabWith n = if n <= 0 then () else (ppStr "\t"; tabWith (n-1)) in
  let tab() = tabWith !tabCount in
  let rec ppBinExp e1 e2 op = ppStr "("; ppExpr e1; ppStr op; ppExpr e2; ppStr ")"
  and ppExpr te =
    let (e,t) = te in
    match e with
    | ILit(v) -> Printf.fprintf outc "%d" v
    | FLit(v) -> Printf.fprintf outc "%f" v
    | SLit(v) -> ppStr ("\"" ^ v ^ "\"")
    | Var(x) -> ppStr ("_"^x)
    | Bexp(op,e1,e2) -> ppBinExp e1 e2 (str_of_binop op)
    (*    (match t with *)
    (*     | TString -> ppStr "concat("; ppExpr e1; ppStr ","; ppExpr e2; ppStr ")" *)
    (* | Diff(e1,e2) ->  *)
    (*    (match t with *)
    (*     | TString -> ppExpr (Sum(e1, (Neg(e2),t)), t) *)
    (*     | _ -> ppBinExp e1 e2 " - ") *)
    (* | Prod(e1,e2) -> ppBinExp e1 e2 " * " *)
    (* | Quot(e1,e2) -> ppBinExp e1 e2 " / " *)
    | Uexp(op,e1) -> ppStr ((str_of_unop op)^"("); ppExpr e1; ppStr ")"
       (* (match t with *)
       (*  | TString -> ppStr "rev("; ppExpr e1; ppStr ")" *)
  in
  let printDecl (Dec(id,typ)) = match typ with
    | TInt -> tab(); ppStr ("int _" ^ id ^ " = 0;\n")
    | TString -> tab(); ppStr ("char* _" ^ id ^ " = \"\";\n") !?!
    | TFloat -> tab(); ppStr ("float _" ^ id ^ " = 0.0;\n")
  in
  let rec ppStmt = function
    | Assign(id,e) -> tab(); ppStr ("_"^id^" = "); ppExpr e; ppStr ";\n"
    | Print(e) ->
       tab(); ppStr "printf(\"";
       (match snd e with
	| TInt -> ppStr "%d\\n\", ";
	| TFloat -> ppStr "%f\\n\", ";
	| TString -> ppStr "%s\\n\", ";
       ); ppExpr e; ppStr ");\n"
    | Read(id) -> 
       (match Ctx.find id gamma with
	| TInt -> tab(); ppStr "scanf(\"%d\", &"
	| TFloat -> tab(); ppStr "scanf(\"%f\", &"
	| TString -> tab(); ppStr ("_"^id^" = malloc(sizeof(char)*32);\n");
		     tab(); ppStr "scanf(\"%s\", "
       ); ppStr ("_"^id^");\n")

    | Ifte(e,xs,ys) ->
       tab(); ppStr "if ("; ppExpr e; ppStr ") {\n"; incr tabCount;
       List.iter ppStmt xs; decr tabCount;
       tab(); ppStr "}\n"; tab(); ppStr "else {\n"; incr tabCount;
       List.iter ppStmt ys; decr tabCount; tab(); ppStr "}\n"

    | Ift(e,xs) ->
       tab(); ppStr "if ("; ppExpr e; ppStr ") {\n"; incr tabCount;
       List.iter ppStmt xs; decr tabCount; tab(); ppStr "}\n"

    | While(e,xs) ->
       tab(); ppStr "while ("; ppExpr e; ppStr ") {\n"; incr tabCount;
       List.iter ppStmt xs; decr tabCount; tab(); ppStr "}\n"
  in
  ppStr cHeader;
  List.iter printDecl decls; println();
  List.iter ppStmt stmts; tab();
  ppStr cTail *)
