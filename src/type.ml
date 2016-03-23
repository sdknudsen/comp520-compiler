(* module Ctx = Map.Make(String) *)
open Ast
open Context
open AuxFunctions


let typecheck_error pos msg = Error.print_error pos ("[typecheck] " ^ msg)

(* add (Iden id) (Var,typ) g *)
(* add (Iden id) (Fun,typ) g *)
(* add (Iden id) (Typ,typ) g *)

(* type context = lvalue Ast.Ctx.t *)

(* type context = string Ast.Ctx.t *)

(* let typClass g = function *)
(*   | TSimp "bool" -> [Bool] *)
(*   | TSimp "int" -> [Numeric,Integer, *)
(*   | TSimp "float64" -> [Numeric, *)
(*   | TSimp "rune" -> [Numeric,Integer, *)

(* let rec str_of_typ = function *)


let typeAST (Prog((pkg,_),decls) : Untyped.ast) : Typed.ast =

  let rec thread f gamma = function (* map, but updated gamma is used for next element *)
    | [] -> ([],gamma)
    | x::xs -> let (d,g) = f gamma x in
               let (tl,gam) = thread f g xs in
               (d::tl,gam)
  in

  let rec tTyp g (t:(string * Lexing.position) annotated_typ): string annotated_typ = match t with
    | TSimp((i,p)) -> if in_context i g
                      then TSimp(i)
                      else typecheck_error p "Use of undefined type"
    | TStruct(tl) -> TStruct(List.map (fun ((i,_), t) -> (i, tTyp g t)) tl)
    | TArray(t,s) -> TArray(tTyp g t, s)
    | TSlice(t) -> TSlice(tTyp g t)
    | TFn(args, rtn) -> TFn(List.map (tTyp g) args, tTyp g rtn)
    | TKind(t) -> TKind(tTyp g t)
    | TVoid -> TVoid
  in

  (* let rec tExpr gamma = function *)
  let rec tExpr g (e,pos) : Typed.annotated_texpr = match e with
    | ILit(d) -> (ILit d, (pos, TSimp "int"))
    | FLit(f) -> (FLit f, (pos, TSimp "float64"))
    | RLit(c) -> (RLit c, (pos, TSimp "rune"))
    | SLit(s) -> (SLit s, (pos, TSimp "string"))
    | Parens(e) -> 
       let (_,(_,typ)) as te = tExpr g e in
       (Parens te, (pos, typ))
    | Bexp(op,e1,e2) -> 
       let (_,(_,typ1)) as te1 = tExpr g e1 in
       let (_,(_,typ2)) as te2 = tExpr g e2 in       
       (*let lub = unify g typ1 typ2 in*)
       let t = TVoid (*(match op with
                | Boolor
                | Booland
                | Equals
                | Notequals	when isComparable lub -> lub
                | Lt
                | Lteq	
                | Gt
                | Gteq		when isOrdered lub -> lub
                | Plus		when isString lub -> lub
                | Plus
                | Minus	
                | Times	
                | Div
                | Modulo	when isNumeric lub -> lub
                | Bitor
                | Bitxor
                | Lshift
                | Rshift
                | Bitand
                | Bitnand 	when isInteger lub -> lub
                | _ -> typecheck_error pos ("Mismatch with '" ^ bop_to_str op ^ "' operation"))

*)
       in (Bexp(op,te1,te2), (pos, t))

    | Uexp(op,e) -> 
       let (_,(_,typ)) as te = tExpr g e in
       let t = (match (typ, op) with
                (* just to start with *)
                | TSimp "int",     Positive -> TSimp "int"
                | TSimp "float64", Positive -> TSimp "float64"
                | TSimp "rune",    Positive -> TSimp "rune"
                | TSimp "int",     Negative -> TSimp "int"
                | TSimp "float64", Negative -> TSimp "float64"
                | TSimp "rune",    Negative -> TSimp "rune"
                | TSimp "bool",    Boolnot  -> TSimp "bool"
                | TSimp "int",     Bitnot   -> TSimp "int"
                | TSimp "rune",    Bitnot   -> TSimp "rune"
                | _ -> typecheck_error pos ("Mismatch with '" ^ uop_to_str op ^ "' operation"))
                 (* change to allow for new types *)
       (* let te = typeExpr gamma e *)
       in (Uexp(op,te), (pos, t))

    | Fn_call(fun_id, es) -> 
       typecheck_error pos ("Function type unimplemented")
      (* let tf = tExpr g f in
       let (fargs,ft) = find f g in
       let tes = List.map (tExpr g) es in
       List.iter (fun (arg,te) ->
           if arg != te.typ
           then raise (TypeError ("Function argument mistmatch between "^typ_to_str arg^" and "^typ_to_str te.typ))
           else ()) (zip fargs tes);
       (Fn_call(tf,tes), (pos, ft)) *)
    (* why does the pdf say that the arguments have to be well typed (they're lvals, ids, or something else?? *)

    | Append((i,ipos) as id, e) ->
(* add (Iden id) (Typ,typ) g *)
       let t = (match find i g with
         | Some(TSlice t) -> t
         | None -> typecheck_error ipos ("variable `" ^ i ^ "` is undefined")
         | _    -> typecheck_error pos  ("\"" ^ i ^ "\" must have type slice"))
       in
       let (_,(_,typ)) as te = tExpr g e in
       if t = typ then (Append(i,te), (pos, TSlice t))
       else typecheck_error pos ("Mismatch in slice between \"" ^ typ_to_str t ^ "\" and \"" ^ typ_to_str typ)

    | Iden((i, ipos) as id) -> begin
        match find i g with
          | Some(t) -> (Iden(i), (pos, t))
          | None -> typecheck_error ipos ("variable `" ^ i ^ "` is undefined")
      end
    | AValue(r,e) ->
       let (_,(_,typ1)) as tr = tExpr g r in
       let (_,(_,typ2)) as te = tExpr g e in
       (* do we allow e to be empty if this is a slice?? *)
       (match typ2 with
         | _ -> typecheck_error pos "Array index must have type int"
         | TSimp "int" -> (AValue(tr,te), (pos, typ1)));

    | SValue(r, id) ->
       let (i,_) = id in
       try
       match tExpr g r with
         | (_,(_, TStruct(tl))) as tr ->
            let (_,field_typ) = List.find (function
                                            | (_,i) -> true
                                            | _ -> false)
                                      tl
            in (SValue(tr,i), (pos, field_typ))
         | _ -> typecheck_error pos "Something very wrong"
       with 
         | Not_found -> typecheck_error pos "Invalid struct field"
  in


  let rec tStmt frt g ((p, pos): Untyped.annotated_utstmt) : Typed.annotated_utstmt = match p with 
    | Assign(xs,es) -> 
       let txs = List.map (tExpr g) xs in
       let tes = List.map (tExpr g) es in
       List.iter
         (fun ((_,(pos,tx)),(_,(_,ty))) ->
           if not (tx == ty)
           then typecheck_error pos "Type mismath in assign")
         (zip txs tes);
       (Assign(txs, tes), pos)
    | Print(es) -> 
      (Print(List.map (tExpr g) es), pos)
    | Println(es) ->
      (Println(List.map (tExpr g) es), pos)
    | If_stmt(po,e,ps,pso) ->
       let tpo = typo (tStmt frt g) po in
       let (_,(_,typ)) as te = tExpr g e in
       if typ != TSimp "bool"
       then typecheck_error pos "If condition must have typ bool"
       else
         let gthen = scope g in
         let tps = List.map (tStmt frt gthen) ps in
         (match pso with
           | Some(ps) ->
               let gesle = scope g in
               (If_stmt(tpo, te, tps, Some((List.map (tStmt frt gesle)) ps)), pos)
           | None -> (If_stmt(tpo, te, tps, None), pos));
    | Switch_stmt(po, eo, ps) -> 
       let tpo = match po with
                  | Some(p) -> Some(tStmt frt g p)
                  | None -> None
       in
       let teo = match eo with
                  | Some(e) -> Some(tExpr g e)
                  | None -> None
       in
       let tps = List.map (tStmt frt g) ps in
       (Switch_stmt(tpo, teo, tps),pos)
    | Switch_clause(Some(exps), ps) ->
       let teso = (List.map (tExpr g) exps) in
       let tps = List.map (tStmt frt g) ps in
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
       let tpo2 = match po2 with
                   | Some(p) -> Some(tStmt frt g p)
                   | None -> None
       in
       let tps  = List.map (tStmt frt g) ps in
       (For_stmt(tpo1, teo, tpo2, tps), pos)

    | Var_stmt(decls) ->
       let tc_vardecl ((i,ipos) as id, e, t) =
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
               if etyp == tt
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
                        if t != te
                        then typecheck_error pos ("Type mismatch with variable `" ^ i ^ "`")
                       end
                   else
                     add i te g)
                 tds;

       (SDecl_stmt(tds), pos)

(*
    | Type_stmt(id_typ_ls) -> 
       (Type_stmt(id_typ_ls),pos)
*) 
    | Expr_stmt e ->
       let te = tExpr g e in
       (Expr_stmt(te),pos)
    | Return(None) ->
       if not (frt == TVoid)
       then typecheck_error pos "Function should return a value"
       else (Return(None),pos)
    | Return(Some(e)) ->
       let (_,(_,typ)) as te = tExpr g e in
       if frt == typ
       then (Return(Some(te)), pos)
       else typecheck_error pos "Unexpected return type"
    | Break -> (Break, pos)
    | Block(s) -> 
       let ts = List.map (tStmt frt (scope g)) s in
       let out = (Block(ts), pos) in
       out;
    | Continue -> (Continue, pos)
    | Empty_stmt -> (Empty_stmt,pos)
    | _ -> (Continue, pos)

  in


  let rec tDecl g ((d,pos): Untyped.annotated_utdecl) : Typed.annotated_utdecl = match d with

    | Var_decl(decls) -> 
       let tc_vardecl ((i,ipos) as id, e, t) =
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
               if etyp == tt
               then tt
               else typecheck_error ipos ("Conflicting type for variable declaration " ^ i)
           | Some((_,(_,etyp))), None -> etyp
         in
         add i tt g;
         (i, te, Some(tt))
       in

       let ls = List.map (List.map tc_vardecl) decls in
       (Var_decl(ls), pos)




    | Type_decl(typId_typ_ls) -> 
       let tl = List.map (fun ((i,_), t) -> (i, tTyp g t)) typId_typ_ls
       in
       (List.iter (fun (num,typ) -> if in_scope num g
                                    then typecheck_error pos ("Type `" ^ num ^ "` already declared in scope")
                                    else add num (TKind(typ)) g)
                  tl);
      
       (Type_decl(tl), pos)

    | Func_decl((fId,_), id_typ_ls, typ, ps) -> (* start by creating a new frame?? *)
       if in_scope fId g
       then typecheck_error pos ("Function \"" ^ fId ^ "\" already declared")
       else begin
         let ng = scope g in  
         let itl = List.map (fun((i,ipos), t) ->
                               let t = tTyp g t in
                               if in_scope i ng
                               then typecheck_error
                                      ipos
                                      ("Parameter name `" ^ i ^ "` use twice")
                               else (add i t ng; (i, t)))
                            id_typ_ls in
         let tl = List.map (fun(_, t) -> t) itl in
         let rtntyp = tTyp g typ in

         add fId (TFn(tl, rtntyp)) g;
         let tps = List.map (tStmt rtntyp ng) ps in
         (Func_decl(fId, itl, rtntyp, tps), pos)
       end
    |_ ->
       typecheck_error pos "Oups";

  and tDecls gamma ds = List.map (tDecl gamma) ds
  in
  Prog(pkg, (tDecls (init()) decls))
