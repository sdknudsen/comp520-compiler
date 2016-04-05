open Ast
open Context
open Ho 
open AuxFunctions
open Printf

let generate table (Prog(id,decls) : Typed.ast) oc =
  let tabc = ref 0 in (* tab count *)
  let pln() = fprintf oc "\n" in (* print line *)
  let pstr s = fprintf oc "%s" s in (* print ocaml string *)
  let pid id = pstr (fst id) in
  let rec tabWith n = if n <= 0 then () else (pstr "  "; tabWith (n-1)) in
  let tab() = tabWith !tabc in
  let psfl s f = (* print string followed list *)
    List.iter (fun y -> f y; pstr s)
  in
  let pssl s f = function (* print string separated list *)
    | [] -> ()
    | x::xs -> f x; List.iter (fun y -> pstr s; f y) xs
  in
  let pcsl f = function (* print comma separated list *)
    | [] -> ()
    | x::xs -> f x; List.iter (fun y -> pstr ", "; f y) xs
  in
  let plsl f = function (* print comma separated list *)
    | [] -> ()
    | x::xs -> f x; List.iter (fun y -> pstr "\n"; f y) xs
  in

  let gUOp op =
    let s = match op with
      | Negative -> "neg"
      | Positive -> ""
      | Boolnot -> failwith "!"
      | Bitnot -> failwith "^"
    in pstr s
  in
  let gBOp op =
    let s = match op with
    | Equals -> "eq"
    | Notequals -> "ne"
    | Lt -> "lt_s"
    | Lteq -> "le_s"
    | Gt -> "gt_s"
    | Gteq -> "ge_s"
    | Plus -> "add"
    | Minus -> "sub"
    | Bitor -> "or"
    | Bitand -> "and"
    | Bitxor -> "xor"
    | Times -> "mul"
    | Div -> "div_s"
    | Modulo -> "rem_s"
    | Lshift -> "shl_s"
    | Rshift -> "shr_s"
    | Boolor -> failwith "||"
    | Booland -> failwith "&&"
    | Bitnand -> failwith "&^"
    in pstr s
  in
  
  let rec name_typ (at:Typed.uttyp) = match at with
    | TSimp("bool", _)    -> "bool"
    | TSimp("int", _)     -> "int"
    | TSimp("float64", _) -> "float64"
    | TSimp("rune", _)    -> "rune"
    | TSimp("string", _)  -> "string"

    | TSimp(_, _)    -> failwith "Named types not yet supported"
    | TStruct(_)
    | TArray(_,_)
    | TSlice(_)
    | TFn(_,_) -> failwith "Structured types not yet supported"
    | TVoid -> ""
    | TKind(a) -> ""
  in
  let rec gTyp (at:Typed.uttyp) = match at with
    (* get wast type before printing !! *)
    | TSimp("bool", _)    -> pstr "i32"
    | TSimp("int", _)     -> pstr "i32"
    | TSimp("float64", _) -> pstr "f64"
    | TSimp("rune", _)    -> pstr "i8"
    | TSimp(_, _)    -> failwith "Named types not yet supported"
    | TStruct(_)
    | TArray(_,_)
    | TSlice(_)
    | TFn(_,_) -> failwith "Structured types not yet supported"
    | TVoid -> ()
    | TKind(a) -> gTyp a
  in
  let rec getSuffix (at:Typed.uttyp) : string = match at with
    (* get wast type before printing !! *)
    | TSimp(t, c) -> "_"^t (* ^"_"^string_of_int (scope_depth c) *)
    | TArray(_,_)
    | TStruct(_)
    | TFn(_,_)
    | TSlice(_)
    | TVoid
    | TKind(_) -> failwith "not yet supported"
(* type:   ( type <var> ) *)
(* type:    ( type <name>? ( func <param>* <result>? ) ) *)
  in
  let rec gExpr ((ue,(pos,typ,ctx)):Typed.annotated_texpr) =
    match ue with
    | Iden(id) -> fprintf oc "(get_local $%t)"
                      (fun c -> pstr (id^getSuffix typ^"_"^(string_of_int (scope_depth (get_scope id ctx)))))
    | AValue(r,e) -> ()
    | SValue(r,id) -> ()
    (* | Parens(e)  -> fprintf oc "(%t)" (fun c -> gExpr e) *)
    | ILit(d) -> fprintf oc "(i32.const %d)" d
    | FLit(f) -> fprintf oc "(f64.const %f)" f
    | RLit(c) -> fprintf oc "(i32.const %d)" (int_of_char c)
    | SLit(s) -> fprintf oc "\"%s\"" s
    | Bexp(op,e1,e2) ->
       fprintf oc "(%t.%t %t %t)"
                      (fun c -> gTyp typ)
                      (fun c -> gBOp op)
                      (fun c -> gExpr e1)
                      (fun c -> gExpr e2)
       
    | Uexp(op,e) -> 
       fprintf oc "(%t.%t %t)"
                      (fun c -> gTyp typ)
                      (fun c -> gUOp op)
                      (fun c -> gExpr e)
    | Fn_call((Iden(i),_), k) -> fprintf oc "(call %t %t)"
                                            (fun c -> pstr i)
                                            (fun c -> ())
    | Fn_call(fun_id, es) -> ()
  (* ( call <var> <expr>* ) *)
  (* ( call_import <var> <expr>* ) ( call_indirect <var> <expr> <expr>* ) *)
    | Append(x, e) -> ()
  in
  let rec getId (ue,(pos,typ,ctx):Typed.annotated_texpr):string =
    match ue with
    | Iden(id) -> id^getSuffix typ^"_"^(string_of_int (scope_depth (get_scope id ctx)))
(*
    | AValue(r,e) -> failwith "getId not implemented for AValue"
    | SValue(r,id) -> failwith "getId not implemented for SValue"
*)
    | _ -> failwith "Found non id in lhs of assignment"
  in
  let rec gStmt ((us, (pos,ctx)): Typed.annotated_utstmt) =
   match us with
    | Assign(xs, es) -> 
       plsl (fun (v,e) -> fprintf oc "(set_local $%t %t)"
                                  (fun c -> pstr (getId v))
                                  (fun c -> gExpr e))
         (zip xs es)
    | Var_stmt(xss) ->
       List.iter (plsl (fun (s,eo,typo) ->
                 fprintf oc "(set_local $%t)"
                         (fun c -> (match (typo,eo) with
                                   | (Some typ,Some e) -> pstr (s^getSuffix typ^"_"^(string_of_int (scope_depth (get_scope s ctx)))); gExpr e
                                   | (None,Some e) -> let (_,(_,typ,_)) = e in
                                                   pstr (s^getSuffix typ^"_"^(string_of_int (scope_depth (get_scope id ctx)))); gExpr e
                                   | (Some typ,None) -> pstr ""
                                   | _ -> failwith "weeding error"
                         )))) xss
       (* let ls = List.map (fun () -> ) (List.concat xss) in *)
       (* plsl (fun (v,e) -> fprintf oc "(set_local $%t %t)" *)
       (*                            (fun c -> pstr (getId v)) *)
       (*                            (fun c -> gExpr e)) ls *)

    | Print(es) -> ()
    | Println(es) -> ()
    | If_stmt(po,e,ps,pso) ->
       may (fun s -> gStmt s; pstr "\n"; tab()) po;
       pstr "(if ";
       gExpr e;
       pstr "\n";
       incr tabc;
       tab();
       pstr "(then\n";
       incr tabc;
       pssl "\n" (fun st -> tab(); gStmt st) ps;
       decr tabc;
       pstr ")";

       may (fun ps ->
             pstr "\n";
             tab();
             pstr "(else\n";
             incr tabc;
             pssl "\n" (fun st -> tab(); gStmt st) ps;
             decr tabc;
             pstr ")")
            pso;
       decr tabc;
       pstr ")"
    | Block(stmts) ->
       pstr "(block\n";
       incr tabc;
       pssl "\n" (fun st -> tab(); gStmt st) stmts;
       decr tabc;
       pstr ")"
                   
  (* ( block <name>? <expr>* ) *)
    | Switch_stmt(po, eo, ps) -> ()
    | Switch_clause(eso, ps) -> ()
    | For_stmt(po1, eo, po2, ps) -> ()
     (*
       may (fun s -> gStmt s; pstr "\n"; tab()) po1;
       pstr "(loop\n";
       incr tabc;
       (fun c-> tab(); defaulto gExpr () eo)
       pssl "\n" (fun st -> tab(); gStmt st) ps;
       decr tabc;
       pstr ")"
                   
        fprintf oc "(loop\n%t%t)"
                (fun c -> incr tabc)
                (fun c -> pssl "\n" (fun st -> tab(); gStmt st) ps)
       *)
  (* ( loop <name1>? <name2>? <expr>* ) *)
    | SDecl_stmt(id_e_ls) ->
        pssl "\n"
             (fun (id, e) ->
              let (_,(_,typ,_)) = e in
              tab();
              fprintf oc "(set_local $%t %t)"
                (fun c -> pstr (id^getSuffix typ^"_"^(string_of_int (scope_depth (get_scope id ctx)))))
                (fun c -> gExpr e))
          id_e_ls
        
    | Type_stmt(id_typ_ls) -> ()
    | Expr_stmt e -> gExpr e        
    | Return(eo) -> 
        fprintf oc "(return %t)"
                (fun c-> defaulto gExpr () eo)
  (* ( return <expr>? ) *)
    | Break -> ()
    | Continue -> ()
    | Empty_stmt -> pstr "nop" (* or should we not do anything? *)
  in
  let rec gDecl ((ud,pos): Typed.annotated_utdecl) = tab(); match ud with
           | Var_decl(xss) -> ()
             (* plsl (fun xs -> *)
             (* pstr "var(\n"; incr tabc; *)
             (* List.iter (fun (s,eso,typo) -> *)
             (*     fprintf oc "%t %t%t\n" *)
             (*                  (fun c -> pstr s) *)
             (*                  (fun c -> may (fun t -> pstr " "; gTyp t) typo) *)
             (*                  (fun c -> may (fun t -> pstr " = "; gExpr t) eso) *)
             (* ) xs; pstr ")"; decr tabc) xss *)

           | Type_decl(id_atyp_ls) -> ()
           | Func_decl(fId, id_typ_ls, typ, ps) -> 
              (* local variables must be declared at the function declaration *)
              (* write a function to go through the branch of the typed ast and gather all the variable declarations, then call it at the beginning *)
              pstr "(func $"; pstr fId;
              incr tabc; pstr "\n";
              psfl "\n"
                (fun (id,typ) ->
                  tab();
                  pstr ("(param $"^id^getSuffix typ^"_"^(string_of_int 1)^" ");
                  gTyp typ;
                  pstr ")")
                id_typ_ls;
              (match typ with
                | TVoid -> ()
                | _     ->
                  tab();
                  pstr "(result ";
                  gTyp typ;
                  pstr ")\n");
              let locals = try Hashtbl.find table fId
                           with | _ -> []
              in
                pssl "\n" (fun (v,d,t,t2) ->
                       tab();
                       fprintf oc "(local $%t %t)"
                               (fun c -> pstr (v^"_"^t^"_"^string_of_int d))
                               (fun c -> gTyp t2))
                     locals; pstr "\n";
              pssl "\n" (fun st -> tab(); gStmt st) ps;
              decr tabc;
              pstr ")";

(* func:   ( func <name>? <type>? <param>* <result>? <local>* <expr>* ) *)
(* result: ( result <type> ) *)
  in
(* module:  ( module <type>* <func>* <import>* <export>* <table>* <memory>? <start>? ) *)
       fprintf oc "(module\n%t)"
       (fun c -> incr tabc;
                 plsl gDecl decls;
                 decr tabc)

(* fix tabbing *)



(* more about webassembly: *)

(* value: <int> | <float> *)
(* var: <int> | $<name> *)
(* name: (<letter> | <digit> | _ | . | + | - | * | / | \ | ^ | ~ | = | < | > | ! | ? | @ | # | $ | % | & | | | : | ' | `)+ *)
(* string: "(<char> | \n | \t | \\ | \' | \" | \<hex><hex>)*" *)

(* type: i32 | i64 | f32 | f64 *)

(* unop:  ctz | clz | popcnt | ... *)
(* binop: add | sub | mul | ... *)
(* relop: eq | ne | lt | ... *)
(* sign: s|u *)
(* offset: offset=<uint> *)
(* align: align=(1|2|4|8|...) *)
(* cvtop: trunc_s | trunc_u | extend_s | extend_u | ... *)
