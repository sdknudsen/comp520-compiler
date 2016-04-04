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

  let gUOp op =
    let s = match op with
      | Negative -> "neg"
      | Positive -> failwith "+"
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
    | Lshift -> "shl_s"
    | Rshift -> "shr_s"
    | Boolor -> failwith "||"
    | Booland -> failwith "&&"
    | Bitnand -> failwith "&^"
    | Modulo -> failwith "%"
    in pstr s
  in
  
  let rec gTyp (at:Typed.uttyp) = match at with
    (* get wast type before printing !! *)
    | TSimp(typ_id) -> ()
    | TStruct(x_typ_ls) -> ()
    | TArray(typ,d) -> ()
    | TSlice(typ) -> ()
    | TVoid -> ()
    | TFn(a,b) -> ()
    | TKind(a) -> gTyp a
(* type:   ( type <var> ) *)
(* type:    ( type <name>? ( func <param>* <result>? ) ) *)
  in
  let rec gExpr ((ue,(pos,typ)):Typed.annotated_texpr) =
    pstr "\n";
    match ue with
    | Iden(id) -> () (* look up in symbol table *)
    | AValue(r,e) -> ()
    | SValue(r,id) -> ()
    (* | Parens(e)  -> fprintf oc "(%t)" (fun c -> gExpr e) *)
    | ILit(d) -> fprintf oc "%d" d
    | FLit(f) -> fprintf oc "%f" f
    | RLit(c) -> fprintf oc "%d" (int_of_char c)
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
    | Fn_call((Iden(i),_), k) -> ()
    | Fn_call(fun_id, es) -> ()
  (* ( call <var> <expr>* ) *)
  (* ( call_import <var> <expr>* ) ( call_indirect <var> <expr> <expr>* ) *)
    | Append(x, e) -> ()
  in
  let rec gStmt ((us, pos): Typed.annotated_utstmt) = match us with
    | Assign(xs, es) -> ()
       (* plsl *)
       (*   (fun (v,e) -> fprintf oc "(set_local %t %t)" *)
                               (* (fun c -> getIndex v) *)
         (* ) *)
         (* (zip xs es) *)
    | Var_stmt(xss) -> ()
    | Print(es) -> ()
    | Println(es) -> ()
    | If_stmt(po,e,ps,pso) ->
       (match po with
        | Some p -> gStmt ((Block(
                                [p; (Block([(If_stmt(None,e,ps,pso),pos)]),pos)]
                           )),pos)
                          (* don't need the block any more becuase of our renaming scheme, leving them here because it doesn't make a difference *)
        | None -> 
          fprintf oc "(if %t\n(then %t)%t)\n"
                         (fun c -> gExpr e; tab())
                         (fun c -> incr tabc;
                                   (* tab(); *)
                                   plsl gStmt ps;
                                   decr tabc)
                         (fun c -> incr tabc;
                                   may (fun ps -> pstr "\n(else";
                                                   plsl gStmt ps;
                                                   pstr ")");
                                   decr tabc))
    | Block(stmts) -> plsl gStmt stmts
  (* ( block <name>? <expr>* ) *)
    | Switch_stmt(po, eo, ps) -> ()
    | Switch_clause(eso, ps) -> ()
    | For_stmt(po1, eo, po2, ps) -> ()
  (* ( loop <name1>? <name2>? <expr>* ) *)
    | SDecl_stmt(id_e_ls) -> ()
    | Type_stmt(id_typ_ls) -> ()
    | Expr_stmt e -> ()
    | Return(eo) -> ()
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
              fprintf oc "(func $%t %t (result %t)\n%t)\n"
                              (fun c -> pstr fId)
                              (fun c -> pstr "")
                              (*(fun c -> pssl " " (fun (id,typ,ind) -> pstr ("(param $"^id^" "); gTyp typ; pstr ")") id_typ_ls)*)
                              (fun c -> gTyp typ)
                              (fun c -> pstr "")
                              (*(fun c -> incr tabc;
                                        (* tab(); *)
                                        pssl "\n" gStmt ps;
                                        decr tabc)*)
              (* failwith "no" *)

(* func:   ( func <name>? <type>? <param>* <result>? <local>* <expr>* ) *)
(* result: ( result <type> ) *)
  in
(* module:  ( module <type>* <func>* <import>* <export>* <table>* <memory>? <start>? ) *)
       fprintf oc "(module\n%t\n)"
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
