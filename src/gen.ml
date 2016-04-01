open Ast
open Context
open Ho 
open AuxFunctions

let generate (Prog(id,decls) : Typed.ast) outc =
  let pstr s = Printf.fprintf outc "%s" s in (* print ocaml string *)
  let rec gTyp (at:Typed.uttyp) = match at with
    | TSimp(typ_id) -> ()
    | TStruct(x_typ_ls) -> ()
    | TArray(typ,d) -> ()
    | TSlice(typ) -> ()
    | TVoid -> ()
    | TFn(a,b) -> ()
    | TKind(a) -> ()
(* type:   ( type <var> ) *)
(* type:    ( type <name>? ( func <param>* <result>? ) ) *)
  in
  let rec gExpr ((ue,(pos,typ)):Typed.annotated_texpr) = match ue with
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
  (* ( call <var> <expr>* ) *)
  (* ( call_import <var> <expr>* ) ( call_indirect <var> <expr> <expr>* ) *)
    | Append(x, e) -> ()
  in
  let rec gStmt ((us, pos): Typed.annotated_utstmt) = match us with
    | Assign(xs, es) -> ()
    | Var_stmt(xss) -> ()
    | Print(es) -> ()
    | Println(es) -> ()
    | If_stmt(po,e,ps,pso) -> ()
  (* ( if <expr> ( then <name>? <expr>* ) ( else <name>? <expr>* )? ) *)
  (* ( if <expr1> <expr2> <expr3>? ) *)
    | Block(stmts) -> ()
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
    | Empty_stmt -> ()
  in
  let rec gDecl ((ud,pos): Typed.annotated_utdecl) = tab(); match ud with
           | Var_decl(xss) -> ()
           | Type_decl(id_atyp_ls) -> ()
           | Func_decl(fId, id_typ_ls, typ, ps) -> ()
(* func:   ( func <name>? <type>? <param>* <result>? <local>* <expr>* ) *)
(* result: ( result <type> ) *)
  in
  (* add header *)
(* module:  ( module <type>* <func>* <import>* <export>* <table>* <memory>? <start>? ) *)
  pln(); pln(); pssl "\n" pDecl decls


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
