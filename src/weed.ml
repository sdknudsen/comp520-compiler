open Ast

let at_most cond lst n =
  (List.length (List.filter cond lst)) <= n


let weed ast = 

  let weed_reserved_words (w,pos) = match w with
      | "int" | "float64" | "string" | "rune" | "bool" | "println" | "print" ->
          Error.print_error pos "Invalid use of reserved word"
      | _ -> ()
  in

  let weed_blank (w,pos) = match w with
      | "_" -> Error.print_error pos "Invalid use of `_`"
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
      | _ -> () (* Other types aren't weedable *)
  in
  let rec weed_expression (expr, pos) in_lhs in_stmt_ctx =
    match (expr, in_lhs, in_stmt_ctx) with

      (* expression statement weeding*)
      | (ILit(_),_,true)
      | (FLit(_),_,true)
      | (SLit(_),_,true)
      | (RLit(_),_,true)
      | (Iden(_),_,true)
      | (AValue(_,_),_,true)
      | (SValue(_,_),_,true)
      | (Bexp(_,_,_),_,true)
      | (Append(_,_),_,true)
      | (Uexp(_,_),_,true) ->
          Error.print_error pos "Unexpected expression in expression statement";

      (* l-value error *)
      | (ILit(_),true,_)
      | (FLit(_),true,_)
      | (SLit(_),true,_)
      | (RLit(_),true,_)
      | (Fn_call(_, _),true,_)
      | (Bexp(_,_,_),true,_)
      | (Append(_,_),true,_)
      | (Uexp(_,_),true,_) ->
          Error.print_error pos "Unexpected expression as lvalue";

      (* | (Parens(e), _, _) ->
          weed_expression e in_lhs false; *)
      | (Fn_call((Iden(("int",_)),_), args), _, _)
      | (Fn_call((Iden(("bool",_)),_), args), _, _)
      | (Fn_call((Iden(("float64",_)),_), args), _, _)
      | (Fn_call((Iden(("rune",_)),_), args), _, _) ->
          List.iter (fun x -> weed_expression x false false) args;
      | (Fn_call(fn, args),false,_) ->
          weed_expression fn false false;
          List.iter (fun x -> weed_expression x false false) args;
      | (Iden(x),false,_) ->
          weed_blank x;
          weed_reserved_words x;
      | (AValue(e1,e2),_,_) ->
          weed_expression e1 false false;
          weed_expression e2 false false;
      | (SValue(e,i),_,_) ->
          weed_blank i;
          weed_expression e false false;
      | (Uexp(op, e),_,_) ->
          weed_expression e false false;
      | (Bexp(op, l, r),_,_) ->
          weed_expression l false false;
          weed_expression r false false;
      | (Append(i,arg),_,_) ->
          weed_blank i;
          weed_expression arg false false
      | _ -> ()

  in
  let rec weed_statement (stmt, pos) in_loop in_switch = 
    match stmt with
      | Assign(lhss, rhss) ->
          List.iter (fun lhs -> weed_expression lhs true false) lhss;
          List.iter (fun rhs -> weed_expression rhs false false) rhss;
      | Print(exprs) 
      | Println(exprs) ->
          List.iter (fun x -> weed_expression x false false) exprs;
      | If_stmt(_, cond, then_clause, else_clause) -> begin
          weed_expression cond false false;
          List.iter (fun x -> weed_statement x in_loop in_switch) then_clause;
          match else_clause with
           | Some(c) ->
             List.iter (fun x -> weed_statement x in_loop in_switch) c;
           | None -> ()
        end
      | Switch_stmt(s, e, cases) ->
          (match s with
            | Some(stmt) -> weed_statement stmt in_loop in_switch;
            | None ->());
          (match e with
            | Some(expr) -> weed_expression expr false false;
            | None -> ());
          if not (at_most
                   (fun (c,_) -> match c with 
                     | Switch_clause(None, _) -> true
                     | _ -> false)
                   cases
                   1)
          then Error.print_error pos "Two default in switch statement";
          List.iter (fun x -> weed_statement x in_loop true) cases;
      | Switch_clause(_, stmts) ->
          List.iter (fun x -> weed_statement x in_loop true) stmts;
      | For_stmt(init, cond, inc, stmts) -> begin
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

      | Var_stmt(l) -> begin
          List.iter
            (List.iter
              (fun (i, e, t) ->
                weed_reserved_words i;
                match t with
                  | None -> ()
                  | Some(t) -> weed_type t;
                match e with
                  | None -> () 
                  | Some(e) -> weed_expression e false false;))
            l
          ; 
        end
      | SDecl_stmt(decls) ->
          List.iter (fun (lhs, rhs) ->
                       weed_reserved_words lhs;
                       weed_blank lhs;
                       weed_expression rhs false false;)
                    decls
      | Type_stmt(tl) ->
          List.iter
            (fun (id, t) ->
               weed_reserved_words id;
               weed_blank id;
               weed_type t) 
            tl
          ;
      | Return(Some(expr)) ->
          weed_expression expr false false;
      | Expr_stmt(expr) -> begin
          (* let rec no_parens x = match x with
           | (Parens(x),_) -> (no_parens x)
           | _ -> x
          in
          match (no_parens expr) with *)
          match expr with
           | (Fn_call(_,_),_) as expr -> weed_expression expr false true
           | _ -> Error.print_error pos "invalid expression statement"
        end
      | Block(stmts) ->
          List.iter (fun x -> weed_statement x in_loop in_switch) stmts;
      | Break ->
          if not in_loop && not in_switch
          then Error.print_error pos "`break` not used in a loop or switch statement"
          else ();
      | Continue ->
          if not in_loop
          then Error.print_error pos "`continue` not used in a loop statement"
          else ();
      | _ -> ()
  in
  let rec weed_declaration (decl, pos)  =
    match decl with
      | Var_decl(il) ->
          List.iter
            (List.iter
              (fun (i, e, t) ->
                weed_reserved_words i;
                match t with
                  | None -> ()
                  | Some(t) -> weed_type t;
                match e with
                  | None -> () 
                  | Some(e) -> weed_expression e false false;))
            il
          ; 
      | Type_decl(tl) ->
          List.iter
            (fun (id, t) ->
               weed_reserved_words id;
               weed_blank id;
               weed_type t) 
            tl
          ;
      | Func_decl(id, args, return_type, stmts) ->
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
