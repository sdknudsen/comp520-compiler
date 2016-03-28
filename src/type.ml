open Ast
open Context
open Ho 
open AuxFunctions

let typecheck_error pos msg = Error.print_error pos ("[typecheck] " ^ msg)

let rec valid_return_path stmts =
  let rec inner ebreak (econt,path_safe) stmt = match stmt with
    | (Return(_),_)  -> (false, true)
    | (Continue,_)   -> (true, false)
    | (Break,_)      -> ebreak
    | (For_stmt(po1, eo, po2, ps),_) -> 
       (List.fold_left (inner (econt, path_safe))
                       (econt, path_safe)
                       (List.rev ps))
      
    | (Switch_stmt(_, _, ps),_) -> 
      let cases = List.map (inner (econt,path_safe) (econt,path_safe)) ps in
      let cont = List.map fst cases in
      let rc = (List.fold_left (fun acc x -> acc && x) true cont) in
      let rp = (List.fold_left (fun acc (tc,tp) -> acc && ((not tc) && tp)) true cases) in
      (rc, rp)

    | (Block(ps),_)
    | (Switch_clause(_, ps),_) ->
      (List.fold_left (inner ebreak)
                      (econt,path_safe)
                      (List.rev ps))
 
    | (If_stmt(po,e,ps,pso),_) ->
       let (tc,tp) = (List.fold_left
                      (inner ebreak)
                      (econt,path_safe)
                      (List.rev ps))
       in
       let (ec,ep) = (defaulto
                      (List.fold_left
                       (inner ebreak)
                       (econt,path_safe))
                      (econt,path_safe)
                      (mapo List.rev pso))
       in
       (tc && ec, ((not tc)&& tp) && ((not ec)&&ep))
    | _ -> (econt, path_safe)

  in
  let (tc,tr) = List.fold_left (inner (false,false)) (false,false) (List.rev stmts) in
  tr

let typeAST (Prog((pkg,_),decls) : Untyped.ast) : Typed.ast =

  let rec tTyp g (t:Untyped.uttyp): Typed.uttyp =
   match t with
    | TSimp((i,p)) -> if in_context i g
                      then TSimp(i, get_scope i g)
                      else typecheck_error p "Use of undefined type"
    | TStruct(tl) -> TStruct(List.map (fun ((i,_), t) -> (i, tTyp g t)) tl)
    | TArray(at,s) -> TArray(tTyp g at, s)
    | TSlice(st) -> TSlice(tTyp g st)
    | TFn(args, rtn) -> TFn(List.map (tTyp g) args, tTyp g rtn)
    | TKind(t) -> TKind(tTyp g t)
    | TVoid -> TVoid
  in

  (* let rec tExpr gamma = function *)
  let rec tExpr g (e,pos) : Typed.annotated_texpr =
  match e with
    | ILit(d) -> (ILit d, (pos, sure (get_type_instance "int" g)))
    | FLit(f) -> (FLit f, (pos, sure (get_type_instance "float64" g)))
    | RLit(c) -> (RLit c, (pos, sure (get_type_instance "rune" g)))
    | SLit(s) -> (SLit s, (pos, sure (get_type_instance "string" g)))
    | Parens(e) -> 
       let (_,(_,typ)) as te = tExpr g e in
       (Parens te, (pos, typ))
    | Bexp(op,e1,e2) -> 
       let (_,(_,typ1)) as te1 = tExpr g e1 in
       let (_,(_,typ2)) as te2 = tExpr g e2 in       
       (* let base = unify g typ1 typ2 in *)
       let base = if same_type typ1 typ2
       then typ1
       else typecheck_error pos "Mismatched type"
       in (* allow for defined types *)
       let t = 
         (match op with
          | Boolor
            | Booland   when isBool base  -> sure (get_type_instance "bool" g)
          | Equals
            | Notequals	when isComparable base -> sure (get_type_instance "bool" g)
          | Lt
            | Lteq	
            | Gt
            | Gteq 	when isOrdered base -> sure (get_type_instance "bool" g)
          | Plus        when isString base  -> base
          | Plus
            | Minus	
            | Times	
            | Div
            | Modulo	when isNumeric base -> base
          | Bitor
            | Bitxor
            | Lshift
            | Rshift
            | Bitand
            | Bitnand 	when isInteger base -> base
          | _ -> typecheck_error pos
                 (Printf.sprintf
                  "Mismatch with '%s' operation: %s %s %s"
                  (bop_to_str op)
                  (typ_to_str typ1)
                  (bop_to_str op)
                  (typ_to_str typ2)))

       in (Bexp(op,te1,te2), (pos, t))

    | Uexp(op,e) -> 
       let (_,(_,typ)) as te = tExpr g e in
       let base = if true then typ else failwith "not done" in (* check defined types *)
       let t = (match op with
                | Positive	when isNumeric base -> base
                | Negative	when isNumeric base -> base
                | Boolnot	when isBool base -> base
                | Bitnot	when isInteger base -> base
                | _ -> typecheck_error pos ("Mismatch with '" ^ uop_to_str op ^ "' operation"))
                 (* change to allow for new types *)
                 (* let te = typeExpr gamma e *)
       in (Uexp(op,te), (pos, t))

   (* | Fn_call((Iden((x,ipos)),_), [e]) when defaulto isCastable false (find x g) ->*)
    | Fn_call((Iden((x,ipos)),_),[e]) when defaulto isCastable false (get_type_instance x g)->
       let (_,(_,t)) as te = tExpr g e in
       if (isCastable t)
       then
         let tx = sure (get_type_instance x g) in 
         (Fn_call((Iden(x), (ipos, TKind(tx))), [te]), (pos, tx))
       else typecheck_error pos ("Type `" ^ (typ_to_str t) ^ "` is not castable")

    | Fn_call(fid, es) -> 
       let (_,(_,t)) as tfid = tExpr g fid in
       let (fargs,ft) = begin
         match t with
          | TFn(fargs,ft) -> (fargs,ft)
          | _ -> ([TVoid], TVoid)
       end in
       let texps = List.map (tExpr g) es in
       let tes = List.map (fun e -> snd (snd e)) texps in
       let typecheck_args ta te = begin
         if not (same_type ta te) 
         then typecheck_error pos ("Function argument mistmatch between "^typ_to_str ta^" and "^typ_to_str te)
         else ()
       end in
       List.iter2 typecheck_args fargs tes;
       (Fn_call(tfid,texps), (pos, ft))

    | Append((i,ipos), e) ->
       let t = (match find i g with
         | Some(TSlice(t)) -> t
         | None -> typecheck_error ipos ("variable `" ^ i ^ "` is undefined")
         | _    -> typecheck_error pos  ("`" ^ i ^ "` must have be a slice"))
       in
       let (_,(_,typ)) as te = tExpr g e in
       if same_type t typ then begin
          (Append(i,te), (pos, TSlice t))
       end else
          typecheck_error pos ("Mismatch in slice between \"" ^ typ_to_str t ^ "\" and \"" ^ typ_to_str typ)

    | Iden(i, ipos) -> begin
        match find i g with
          | Some(t) -> (Iden(i), (pos, t))
          | None -> typecheck_error ipos ("variable `" ^ i ^ "` is undefined")
      end
    | AValue(r,e) ->
       let (_,(p1,typ1)) as tr = tExpr g r in
       let (_,(p2,typ2)) as te = tExpr g e in
       (* do we allow e to be empty if this is a slice?? *)
       let ot = (match typ1 with
         | TArray(t,_) -> t
         | TSlice(t) -> t
         | _ -> typecheck_error p1 "Non-indexable value");
       in
       (if same_type typ2 (sure (get_type_instance "int" g))
        then (AValue(tr,te), (pos, ot))
        else typecheck_error p2 "Array index must have type int");

    | SValue(e, (i,ip)) ->
       let te = tExpr g e in
       let etyp = snd (snd te) in
       let btyp = sure (get_base_type etyp) in
       
       let (_,ftyp) = (match btyp with
         | TStruct(tl) -> begin
             try
               List.find (function | (_,i) -> true)
                         tl
             with 
              |   Not_found -> typecheck_error ip ("Invalid struct field `"^i^"`")
           end
         | TKind(_) -> typecheck_error pos "Expression of type kind"
         | _ -> typecheck_error pos "Expression not of type struct")
       in
       (SValue(te,i), (pos, ftyp))
  in


  let rec tStmt frt g ((p, pos): Untyped.annotated_utstmt) : Typed.annotated_utstmt = match p with 
    | Assign(xs,es) -> 
       (* let txs = List.map (tExpr g) xs in *)
       let tes = List.map (tExpr g) es in
       let zipped = zip xs tes in
       let check_assign = function
         | ((Iden(("_",p)),_), (e,(ep,et))) -> (Iden("_"),(p,et))
         | (lhs, (e,(ep,et))) ->
              let (_,(_,t)) as tlhs = (tExpr g lhs) in
              if not (same_type t et)
              then typecheck_error pos "Type mismatch in assign"
              else tlhs
       in
       let txs = List.map check_assign zipped in
       (Assign(txs, tes), pos)
    | Print(es) -> 
      let texps = List.map (tExpr g) es in
      (List.iter (fun (_,(p,t)) ->
                   if not (isBaseType t)
                   then typecheck_error p "Print argument not of base type") texps);
      (Print(texps), pos)
    | Println(es) ->
      let texps = List.map (tExpr g) es in
      (List.iter (fun (_,(p,t)) ->
                   if not (isBaseType t)
                   then typecheck_error p "Print argument not of base type") texps);
      (Println(texps), pos)
    | If_stmt(po,e,ps,pso) ->
       let tinit = mapo (tStmt frt g) po in
       let (_,(_,typ)) as tcond = tExpr g e in
       if not (same_type typ (sure (get_type_instance "bool" g)))
       then typecheck_error pos "If condition must have type bool";
       let gthen = scope g in
       let tthen = List.map (tStmt frt gthen) ps in
       unscope gthen;
       let gelse = scope g in
       let telse = (match pso with
           | Some(ps) -> Some((List.map (tStmt frt gelse)) ps)
           | None -> None)
       in
       unscope gelse;
       (If_stmt(tinit, tcond, tthen, telse), pos)

    | Switch_stmt(po, None, ps) -> 
       let tpo = mapo (fun p -> tStmt frt g p) po in
       let check_clause = function
         | (Switch_clause(Some(exps), ps),p) ->
             let es = List.map (tExpr g) exps in
             List.iter
              (fun (_,(p,t)) ->
                 if not (same_type t (sure (get_type_instance "bool" g)))
                 then typecheck_error p "Cases expression does not match switch")
               es;
             let g' = scope g in
             (Switch_clause(Some(es), List.map (tStmt frt g') ps), p)
         | (Switch_clause(None, ps),p) ->
             let g' = scope g in
             (Switch_clause(None, List.map (tStmt frt g') ps), p)
         | (_,p) -> typecheck_error p "Unexpected statement in switch clause"
       in

       let clauses = List.map check_clause ps in
       (Switch_stmt(tpo, None, clauses),pos)

    | Switch_stmt(po, Some(exp), ps) -> 
       let tpo = mapo (fun p -> tStmt frt g p) po in
       let (_,(_,et)) as teo = (fun e -> tExpr g e) exp in

       let check_clause = function
         | (Switch_clause(Some(exps), ps),p) ->
             let es = List.map (tExpr g) exps in
             List.iter
              (fun (_,(p,t)) ->
                 if not (same_type t et)
                 then typecheck_error p "Cases expression does not match switch")
               es;
             let g' = scope g in
             (Switch_clause(Some(es), List.map (tStmt frt g') ps), p)
         | (Switch_clause(None, ps),p) ->
             let g' = scope g in
             (Switch_clause(None, List.map (tStmt frt g') ps), p)
         | (_,p) -> typecheck_error p "Unexpected statement in switch clause"
       in

       let clauses = List.map check_clause ps in
       (Switch_stmt(tpo, Some(teo), clauses),pos)
(*
       let tps = List.map (tStmt frt g) ps in
       (* check that parent has same type as children *)
       let _ = (match teo,ps with
                | Some(pare), [Switch_clause(Some(exps), ps),pos1] ->
                   let g' = scope g in
                   let teso = pare::(List.map (tExpr g') exps) in
                   let _ = if all_same (fun x -> snd (snd x)) teso then ()
                           else typecheck_error pos1 "Type mismatch in switch" in
                   ()
                | _ -> ()) in
*)
    (* at some point, we don't need to have the the following two cases, leaving them here for now *)
    | Switch_clause(Some(exps), ps) ->
       let g' = scope g in
       let teso = (List.map (tExpr g') exps) in
       let _ = if all_same (fun x -> snd (snd x)) teso then ()
               else typecheck_error pos "Type mismatch in switch" in
       let tps = List.map (tStmt frt g') ps in
       (Switch_clause(Some(teso), tps),pos)
    | Switch_clause(None, ps) ->
       let tps = List.map (tStmt frt g) ps in
       (Switch_clause(None, tps),pos)

    | For_stmt(po1, eo, po2, ps) -> 
       let tpo1 = match po1 with
                   | Some(p) -> Some(tStmt frt g p)
                   | None -> None
       in
       let teo  = mapo (tExpr g) eo in

       (match teo with 
         | None -> ()
         | Some(_,(_,t)) when (same_type t (sure (get_type_instance "bool" g))) -> ()
         | _ -> typecheck_error pos "Condition is not a boolean expression");

       let tpo2 = (match po2 with
                   | Some(p) -> Some(tStmt frt g p)
                   | None -> None)
       in
       let ng = scope g in
       let tps  = List.map (tStmt frt ng) ps in
       (For_stmt(tpo1, teo, tpo2, tps), pos)

    | Var_stmt(decls) ->
       let tc_vardecl ((i,ipos), e, t) =
         if in_scope i g then typecheck_error ipos ("Variable \""^ i ^"\" already declared in scope");
         
         let te = match e with
           | None -> None
           | Some(e) -> Some(tExpr g e)
         in
         let tt = match te, t with
           | None, None -> typecheck_error ipos "Neither type or expression provided to variable declaration"
           | None, Some(t) -> tTyp g t
           | Some((e,(_,etyp))), Some(t) ->
               let tt = tTyp g t in 
               if same_type etyp tt
               then tt
               else typecheck_error ipos ("Conflicting type for variable declaration `" ^ i ^ "`")
           | Some((_,(_,etyp))), None -> etyp
         in
         add i tt g;
         (i, te, Some(tt))
       in

       let ls = List.map (List.map tc_vardecl) decls in
       (Var_stmt(ls), pos)

    | SDecl_stmt(ds) ->
       let tds = List.map (fun ((i,_), e) -> (i, tExpr g e)) ds in

       if not (List.exists (function
                             | ("_", _) -> false
                             | (i, _) ->  not (in_scope i g))
                           tds)
       then typecheck_error pos "Short declaration should define at least one new variable";

       List.iter (fun (i, (e,(_,te))) ->
                   if (in_scope i g)
                   then match find i g with
                     | None -> () (* Impossible *)
                     | Some(t) -> begin
                        if not (same_type t te)
                        then typecheck_error pos ("Type mismatch with variable `" ^ i ^ "`")
                       end
                   else
                     add i te g)
                 tds;

       (SDecl_stmt(tds), pos)

    | Type_stmt(typ_decls) -> 
       let tl = List.map (fun (i, t) -> (i,TKind(tTyp g t))) typ_decls in
       let tl = List.map
                  (fun ((i,ipos), t) ->
                    if in_scope i g
                    then typecheck_error ipos ("Type `" ^ i ^ "` already declared in scope")
                    else (add i t g; (i,t)))
                  tl
       in
       (Type_stmt(tl), pos)
    | Expr_stmt e ->
       let te = tExpr g e in
       (Expr_stmt(te),pos)
    | Return(None) ->
       if not (same_type frt TVoid)
       then typecheck_error pos "Function should return a value"
       else (Return(None),pos)
    | Return(Some(e)) ->
       let (_,(_,typ)) as te = tExpr g e in
       if same_type frt typ
       then (Return(Some(te)), pos)
       else typecheck_error pos "Unexpected return type"
    | Break -> (Break, pos)
    | Block(stmts) ->
       let ng = scope g in 
       let tstmts = List.map (tStmt frt ng) stmts in
       unscope ng;
       (Block(tstmts), pos)
    | Continue -> (Continue, pos)
    | Empty_stmt -> (Empty_stmt,pos)

  in


  let rec tDecl g ((d,pos): Untyped.annotated_utdecl) : Typed.annotated_utdecl = match d with

    | Var_decl(decls) -> 
       let tc_vardecl ((i,ipos), e, t) =
         let te = match e with
           | None -> None
           | Some(e) -> Some(tExpr g e)
         in

         if in_scope i g then typecheck_error ipos ("Variable \""^ i ^"\" already declared in scope");
         
         let tt = match te, t with
           | None, None -> typecheck_error ipos "Neither type or expression provided to variable declaration"
           | None, Some(t) -> tTyp g t
           | Some((e,(_,etyp))), Some(t) ->
               let tt = tTyp g t in 
               if same_type etyp tt
               then tt
               else typecheck_error ipos ("Conflicting type for variable declaration " ^ i)
           | Some((_,(_,etyp))), None -> etyp
         in
         add i tt g;
         (i, te, Some(tt))
       in

       let ls = List.map (List.map tc_vardecl) decls in
       (Var_decl(ls), pos)




    | Type_decl(typ_decls) -> 
       let tl = List.map (fun (i, t) -> (i,TKind(tTyp g t))) typ_decls in
       let tl = List.map
                  (fun ((i,ipos), t) ->
                    if in_scope i g
                    then typecheck_error ipos ("Type `" ^ i ^ "` already declared in scope")
                    else (add i t g; (i,t)))
                  tl
       in
       (Type_decl(tl), pos)

    | Func_decl((fId,_), args, typ, stmts) ->
       if in_scope fId g
       then typecheck_error pos ("Function \"" ^ fId ^ "\" already declared")
       else begin

         let targs = List.map (fun((i,ipos), t) -> ((i,ipos), tTyp g t)) args in
         let tl = List.map snd targs in
         let rtntyp = tTyp g typ in

         add fId (TFn(tl, rtntyp)) g;

         let ng = scope g in  
         let targs = List.map (fun((i,ipos),t) ->
                               if in_scope i ng
                               then typecheck_error
                                      ipos
                                      ("Parameter name `" ^ i ^ "` use twice")
                               else (add i t ng; (i, t)))
                              targs
         in

         let tstmts = List.map (tStmt rtntyp ng) stmts in
         (match typ with
          | TVoid -> ()
          | _ -> if not (valid_return_path tstmts)
                 then typecheck_error pos ("Execution paths with no returns in function"));

         unscope ng;

         (Func_decl(fId, targs, rtntyp, tstmts), pos)
       end

  and tDecls gamma ds = List.map (tDecl gamma) ds
  in
  let ctx = (init ()) in begin
    add "int"     (TKind (TSimp("#",ctx))) ctx;
    add "bool"    (TKind (TSimp("#",ctx))) ctx;
    add "string"  (TKind (TSimp("#",ctx))) ctx;
    add "rune"    (TKind (TSimp("#",ctx))) ctx;
    add "float64" (TKind (TSimp("#",ctx))) ctx;
    add "true"    (sure (get_type_instance "bool" ctx)) ctx;
    add "false"   (sure (get_type_instance "bool" ctx)) ctx;
    let decls = tDecls ctx decls in
    unscope ctx;
    Prog(pkg, decls)
  end

