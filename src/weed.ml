open Ast

let at_most cond lst n =
  (List.length (List.filter cond lst)) <= n


let weed ast =

  let weed_reserved_words w = match w with
      | "int" | "float64" | "string" | "rune" | "bool" ->
          raise (Error.CompileError "Invalid use of reserved word")
      | _ -> ()
  in

  let weed_blank w = match w with
      | "_" -> raise (Error.CompileError "Invalid use of `_`")
      | _ -> ()
  in

  let weed_binop op l r = match op, l, r with
      | Div, l, ILit(0) -> raise (Error.CompileError "Division by 0")
      | _, _, _ -> ()
  in
  let rec weed_expression expr in_lhs =
    match expr with
      | Lvalue(_) -> () 
      | ILit(_) | FLit(_) | BLit(_) | RLit(_) | SLit(_) -> ()
      | Uexp(op, e) ->
          weed_expression e false;
      | Bexp(op, l, r) ->
          weed_binop op l r;
          weed_expression l false;
          weed_expression r false
      | Fn_call(fn, args) ->
          (* weed_expression fn false; *)
          List.iter (fun x -> weed_expression x false) args
      | Append(_,arg) ->
         weed_expression arg false

  in
  let rec weed_statement stmt in_loop in_switch = 
    match stmt with
      | Assign(a) -> ()
      | Print(exprs) -> ()
      | Println(exprs) -> ()
      | If_stmt(stmt, cond, then_clause, else_clause) ->
          weed_expression cond false;
          List.iter (fun x -> weed_statement x in_loop in_switch) then_clause;
          List.iter (fun x -> weed_statement x in_loop in_switch) then_clause;
      | Switch_stmt(init, value, cases) ->
          if not (at_most
                   (fun c -> match c with 
                     | Switch_clause(None, _) -> true
                     | _ -> false)
                   cases
                   1)
          then raise (Error.CompileError "Two default in switch statement")
          List.iter (fun x -> weed_statement x in_loop true) cases;
      | Switch_clause(cases, stmts) ->
          List.iter (fun x -> weed_statement x in_loop true) stmts;
      | For_stmt(init, cond, inc, stmts) ->
          List.iter (fun x -> weed_statement x true in_switch) stmts;
      | Var_stmt(_) -> ()
      | SDecl_stmt(_) ->()
      | Type_stmt(tl) -> ()
      | Return(expr) -> ()
      | Expr_stmt(expr) ->
          weed_expression expr false
      | Block(stmts) ->
          List.iter (fun x -> weed_statement x in_loop in_switch) stmts;
      | Break ->
          if not in_loop && not in_switch
          then raise (Error.CompileError "Weeding: Break not used in a loop or switch")
          else ()
      | Empty_stmt -> ()
      | Continue ->
          if not in_loop
          then raise (Error.CompileError "Weeding: continue not used in a loop")
          else ()
      | Empty_stmt -> ()
  in
  let rec weed_declaration decl =
    match decl with
      | Var_decl(_) -> () (* (ids * exprs * typ option) list *)
      | Type_decl(_) -> () (* (typ_id * typ) list *)
      | Func_decl(id, args, return_type, stmts) ->
          List.iter (fun x -> weed_statement x false false) stmts;
  in

  match ast with
    | Prog (package, decls) ->
        weed_reserved_words package;
        weed_blank package;
        List.iter (fun x -> weed_declaration x) decls
