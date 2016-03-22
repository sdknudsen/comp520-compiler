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

  let weed_binop expr = match expr with
      | Bexp(Div, l, ILit(0,_), {Untyped.Info.pos}) -> raise (Error.CompileError "Division by 0")
      | _ -> ()
  in
  let rec weed_type typ = match typ with 
      | TSimp(i) -> weed_blank i;
      | TStruct(il) ->
          List.iter
            (fun (i, t) ->
               weed_blank i;
               weed_type t)
            il
          ;
      | TArray(t,i) -> begin
          weed_type t;
          if i <= 0
          then raise (Error.CompileError "Index must be greater than 0")
        end
      | TSlice(t) -> weed_type t;
      | Void -> ()
  in
  let rec weed_expression expr in_lhs in_stmt_ctx =
    match (expr, in_lhs, in_stmt_ctx) with

      (* expression statement weeding*)
      | (ILit(_,{Untyped.Info.pos}),_,true)
      | (FLit(_,{Untyped.Info.pos}),_,true)
      | (SLit(_,{Untyped.Info.pos}),_,true)
      | (RLit(_,{Untyped.Info.pos}),_,true)
      | (BLit(_,{Untyped.Info.pos}),_,true)
      | (Iden(_,{Untyped.Info.pos}),_,true)
      | (AValue(_,_,{Untyped.Info.pos}),_,true)
      | (SValue(_,_,{Untyped.Info.pos}),_,true)
      | (Bexp(_,_,_,{Untyped.Info.pos}),_,true)
      | (Append(_,_,{Untyped.Info.pos}),_,true)
      | (Uexp(_,_,{Untyped.Info.pos}),_,true) ->
          Error.print_error pos "Unexpected expression in expression statement";

      (* l-value error *)
      | (ILit(_,{Untyped.Info.pos}),true,_)
      | (FLit(_,{Untyped.Info.pos}),true,_)
      | (SLit(_,{Untyped.Info.pos}),true,_)
      | (RLit(_,{Untyped.Info.pos}),true,_)
      | (BLit(_,{Untyped.Info.pos}),true,_)
      | (Fn_call(_, _, {Untyped.Info.pos}),true,_)
      | (Bexp(_,_,_,{Untyped.Info.pos}),true,_)
      | (Append(_,_,{Untyped.Info.pos}),true,_)
      | (Uexp(_,_,{Untyped.Info.pos}),true,_) ->
          Error.print_error pos "Unexpected expression as lvalue";

      | (Parens(e,_), _, _) ->
          weed_expression e in_lhs false;
      | (Fn_call(Iden(("int",_),_), args, _), _, _)
      | (Fn_call(Iden(("bool",_),_), args, _), _, _)
      | (Fn_call(Iden(("float64",_),_), args, _), _, _)
      | (Fn_call(Iden(("rune",_),_), args, _), _, _) ->
          List.iter (fun x -> weed_expression x false false) args;
      | (Fn_call(fn, args, _),false,_) ->
          weed_expression fn false false;
          List.iter (fun x -> weed_expression x false false) args;
      | (Iden(x,_),false,_) ->
          weed_blank x;
          weed_reserved_words x;
      | (AValue(e1,e2,_),_,_) ->
          weed_expression e1 false false;
          weed_expression e2 false false;
      | (SValue(e,i,_),_,_) ->
          weed_blank i;
          weed_expression e false false;
      | (Uexp(op, e, _),_,_) ->
          weed_expression e false false;
      | (Bexp(op, l, r, _),_,_) ->
          weed_binop expr;
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
      | Switch_stmt(s, e, cases, {Untyped.Info.pos}) ->
          (match s with
            | Some(stmt) -> weed_statement stmt in_loop in_switch;
            | None ->());
          (match e with
            | Some(expr) -> weed_expression expr false false;
            | None -> ());
          if not (at_most
                   (fun c -> match c with 
                     | Switch_clause(None, _, _) -> true
                     | _ -> false)
                   cases
                   1)
          then Error.print_error pos "Two default in switch statement";
          List.iter (fun x -> weed_statement x in_loop true) cases;
      | Switch_clause(_, stmts,_) ->
          List.iter (fun x -> weed_statement x in_loop true) stmts;
      | For_stmt(init, cond, inc, stmts,_) -> begin
          (match init with
            | Some(i) -> weed_statement i in_loop in_switch;
            | None -> ());
          (match cond with
            | Some(c) -> weed_expression c false false;
            | None -> ());
          (match inc with
            | Some(i) -> weed_statement i in_loop in_switch;
            | None -> ());
          List.iter (fun x -> weed_statement x true in_switch) stmts;
        end

      | Var_stmt(l,_) -> begin
          List.iter (fun (ids, exps, t) ->
            List.iter (fun i -> weed_reserved_words i) ids;
            match t with
              | None -> ()
              | Some(t) -> weed_type t;
            match exps with
              | Some(exps) ->
                 List.iter (fun e -> weed_expression e false false) exps;
              | None -> ();)
          l;
        end
      | SDecl_stmt(ids,None,{Untyped.Info.pos}) ->
          List.iter (fun lhs -> weed_reserved_words lhs) ids;
          List.iter (fun lhs -> weed_blank lhs) ids;
      | SDecl_stmt(ids,Some(exps),{Untyped.Info.pos}) ->
          List.iter (fun lhs -> weed_reserved_words lhs) ids;
          List.iter (fun lhs -> weed_blank lhs) ids;
          List.iter (fun rhs -> weed_expression rhs false false) exps;
      | Type_stmt(tl,_) ->
          List.iter
            (fun (id, t) ->
               weed_reserved_words id;
               weed_blank id;
               weed_type t) 
            tl
          ;
      | Return(Some(expr),_) ->
          weed_expression expr false false;
      | Expr_stmt(expr,_) ->
          weed_expression expr false true;
      | Block(stmts,_) ->
          List.iter (fun x -> weed_statement x in_loop in_switch) stmts;
      | Break({Untyped.Info.pos}) ->
          if not in_loop && not in_switch
          then Error.print_error pos "`break` not used in a loop or switch statement"
          else ();
      | Continue({Untyped.Info.pos}) ->
          if not in_loop
          then Error.print_error pos "`continue` not used in a loop statement"
          else ();
      | _ -> ()
  in
  let rec weed_declaration decl =
    match decl with
      | Var_decl(il,_) ->
          List.iter
            (fun (ids, exps, typ) ->
              List.iter (fun id -> weed_reserved_words id) ids;
              match typ with
                | None -> ()
                | Some(t) -> weed_type t;
              match exps with
                | None -> () 
                | Some(es) -> List.iter (fun e -> weed_expression e false false) es;)
            il
          ; 
      | Type_decl(tl,_) ->
          List.iter
            (fun (id, t) ->
               weed_reserved_words id;
               weed_blank id;
               weed_type t) 
            tl
          ;
      | Func_decl(id, args, return_type, stmts, {Untyped.Info.pos}) ->
          weed_blank id;
          weed_reserved_words id;
          weed_type return_type;
          List.iter (fun (id, typ) -> weed_reserved_words id; weed_blank id) args; 
          List.iter (fun x -> weed_statement x false false) stmts
  in

  match ast with
    | Prog (package, decls) ->
        weed_reserved_words package;
        weed_blank package;
        List.iter (fun x -> weed_declaration x) decls
