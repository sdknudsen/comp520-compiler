open Ast

let at_most cond lst n =
  (List.length (List.filter cond lst)) <= n


let weed ast = 

  let weed_reserved_words (w,{Untyped.Info.pos}) = match w with
      | "int" | "float64" | "string" | "rune" | "bool" ->
          Error.print_error pos "Invalid use of reserved word"
      | _ -> ()
  in

  let weed_blank (w,{Untyped.Info.pos}) = match w with
      | "_" -> Error.print_error pos "Invalid use of `_`"
      | _ -> ()
  in

  (* let weed_binop expr = match expr with
      | Bexp(Div, l, ILit(0,_), {Untyped.Info.pos}) -> raise (Error.CompileError "Division by 0")
      | _ -> ()
  in *)
  let rec weed_expression expr in_lhs in_stmt_ctx =
    match (expr, in_lhs, in_stmt_ctx) with

      | (ILit(_,{Untyped.Info.pos}),_,true)
      | (FLit(_,{Untyped.Info.pos}),_,true)
      | (SLit(_,{Untyped.Info.pos}),_,true)
      | (RLit(_,{Untyped.Info.pos}),_,true)
      | (BLit(_,{Untyped.Info.pos}),_,true)
      | (Iden(_,{Untyped.Info.pos}),_,true)
      | (Iden(_,{Untyped.Info.pos}),_,true)
      | (AValue(_,_,{Untyped.Info.pos}),_,true)
      | (SValue(_,_,{Untyped.Info.pos}),_,true)
      | (Bexp(_,_,_,{Untyped.Info.pos}),_,true)
      | (Append(_,_,{Untyped.Info.pos}),_,true)
      | (Uexp(_,_,{Untyped.Info.pos}),_,true) ->
          Error.print_error pos "Unexpected expression in expression statement";
      | (Fn_call(_, _, {Untyped.Info.pos}),true,_)
      | (Bexp(_,_,_,{Untyped.Info.pos}),true,_)
      | (Append(_,_,{Untyped.Info.pos}),true,_)
      | (Uexp(_,_,{Untyped.Info.pos}),true,_) ->
          Error.print_error pos "Unexpected expression as lvalue";

      | (Fn_call(fn, args, _),false,_) ->
          weed_expression fn false false;
          List.iter (fun x -> weed_expression x false false) args;
      | (Iden(x,_),false,_) ->
          weed_blank x;
      | (AValue(e1,e2,_),_,_) ->
          weed_expression e1 false false;
          weed_expression e2 false false;
      | (SValue(e,i,_),_,_) ->
          weed_blank i;
          weed_expression e false false;
      | (Uexp(op, e, _),_,_) ->
          weed_expression e false false;
      | (Bexp(op, l, r, _),_,_) ->
          (* weed_binop expr; *)
          weed_expression l false false;
          weed_expression r false false;
      | (Append(i,arg,_),_,_) ->
          weed_blank i;
          weed_expression arg false false

      | _ -> ()

  in
  let rec weed_statement stmt in_loop in_switch = 
    match stmt with
      | Assign(lhss, rhss, _) ->
          List.iter (fun lhs -> weed_expression lhs true false) lhss;
          List.iter (fun rhs -> weed_expression rhs false false) rhss;
      | Print(exprs,_) 
      | Println(exprs,_) ->
          List.iter (fun x -> weed_expression x false false) exprs;
      | If_stmt(_, cond, then_clause, else_clause,_) ->
          weed_expression cond false false;
          List.iter (fun x -> weed_statement x in_loop in_switch) then_clause;
          List.iter (fun x -> weed_statement x in_loop in_switch) then_clause;
      | Switch_stmt(_, _, cases, {Untyped.Info.pos}) ->
          if not (at_most
                   (fun c -> match c with 
                     | Switch_clause(None, _, _) -> true
                     | _ -> false)
                   cases
                   1)
          then Error.print_error pos "Two default in switch statement"
          List.iter (fun x -> weed_statement x in_loop true) cases
      | Switch_clause(_, stmts,_) ->
          List.iter (fun x -> weed_statement x in_loop true) stmts;
      | For_stmt(init, cond, inc, stmts,_) ->
          List.iter (fun x -> weed_statement x true in_switch) stmts;
(*
      | Var_stmt(_,_) -> ()
      | SDecl_stmt(_,_) ->()
*)
      | Type_stmt(tl,{Untyped.Info.pos}) ->
          List.iter
            (fun (iden, _) ->
              weed_reserved_words iden)
            tl
      | Return(Some(expr),_) ->
          weed_expression expr false false
      | Expr_stmt(expr,_) ->
          weed_expression expr false true
      | Block(stmts,_) ->
          List.iter (fun x -> weed_statement x in_loop in_switch) stmts;
      | Break({Untyped.Info.pos}) ->
          if not in_loop && not in_switch
          then Error.print_error pos "`break` not used in a loop or switch statement"
          else ()
(*
      | Empty_stmt(_) -> ()
*)
      | Continue({Untyped.Info.pos}) ->
          if not in_loop
          then Error.print_error pos "`continue` not used in a loop statement"
          else ()
      | _ -> ()
  in
  let rec weed_declaration decl =
    match decl with
      | Var_decl(_,_) -> () (* (ids * exprs * typ option) list *)
      | Type_decl(_,_) -> () (* (typ_id * typ) list *)
      | Func_decl(id, args, return_type, stmts, {Untyped.Info.pos}) ->
          weed_blank id;
          weed_reserved_words id;
          List.iter (fun (id, typ) -> weed_reserved_words id; weed_blank id) args; 
          List.iter (fun x -> weed_statement x false false) stmts
  in

  match ast with
    | Prog (package, decls) ->
        weed_reserved_words package;
        weed_blank package;
        List.iter (fun x -> weed_declaration x) decls
