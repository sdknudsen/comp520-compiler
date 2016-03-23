(* module Ctx = Map.Make(String) *)
open Ast
open Context
open AuxFunctions
open Errors

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


let typeAST (Prog(pkg,decls) : Untyped.ast) : Typed.ast =

  let rec thread f gamma = function (* map, but updated gamma is used for next element *)
    | [] -> ([],gamma)
    | x::xs -> let (d,g) = f gamma x in
               let (tl,gam) = thread f g xs in
               (d::tl,gam)
  in
  (* let rec tExpr gamma = function *)
  let rec tExpr g (e,pos) : Typed.annotated_texpr = match e with
    | ILit(d) -> (ILit d, (pos, TSimp "int"))
    | FLit(f) -> (FLit f, (pos, TSimp "float64"))
    (* | BLit(b) -> { exp = BLit b ; typ = TSimp "bool" } *) (* why is bool not included in the pdf?? *)
    | RLit(c) -> (RLit c, (pos, TSimp "rune"))
    | SLit(s) -> (SLit s, (pos, TSimp "string"))
    | Parens(e) -> 
       let (_,(_,typ)) as te = tExpr g e in
       (Parens te, (pos, typ))
    | Bexp(op,e1,e2) -> 
       let (_,(_,typ1)) as te1 = tExpr g e1 in
       let (_,(_,typ2)) as te2 = tExpr g e2 in
    (*   let lub = unify g typ1 typ1 in *)
       let t = Void
(*
               (match op with
                | Boolor
                | Booland	when isBool lub -> lub
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
                | _ -> raise (TypeError ("Mismatch with '" ^ bop_to_str op ^ "' operation")))
*)
                (* | TSimp "int", TSimp "int", Plus -> TSimp "int" *)
                (* | TSimp "int", TSimp "int", Plus -> TSimp "int") *)

       (* in { exp = Bexp(op,te1,te2) ; typ = t } *)
       in (Bexp(op,te1,te2), (pos, t))

    | Uexp(op,e) -> 
       let (_,(_,typ)) as te = tExpr g e in
       let t = (match (typ, op) with
                (* just to start with *)
                | TSimp "int", Positive -> TSimp "int"
                | TSimp "float64", Positive -> TSimp "float64"
                | TSimp "rune", Positive -> TSimp "rune"
                | TSimp "int", Negative -> TSimp "int"
                | TSimp "float64", Negative -> TSimp "float64"
                | TSimp "rune", Negative -> TSimp "rune"
                | TSimp "bool", Boolnot -> TSimp "bool"
                | TSimp "int", Bitnot -> TSimp "int"
                | TSimp "rune", Bitnot -> TSimp "rune"
                | _ -> raise (TypeError ("Mismatch with '" ^ uop_to_str op ^ "' operation")))
                 (* change to allow for new types *)
       (* let te = typeExpr gamma e *)
       in (Uexp(op,te), (pos, t))

    | Fn_call(fun_id, es) -> 
       raise (TypeError ("Function type unimplemented"))
      (* let tf = tExpr g f in
       let (fargs,ft) = find f g in
       let tes = List.map (tExpr g) es in
       List.iter (fun (arg,te) ->
           if arg != te.typ
           then raise (TypeError ("Function argument mistmatch between "^typ_to_str arg^" and "^typ_to_str te.typ))
           else ()) (zip fargs tes);
       (Fn_call(tf,tes), (pos, ft)) *)
    (* why does the pdf say that the arguments have to be well typed (they're lvals, ids, or something else?? *)

    | Append((x,_) as id, e) ->
(* add (Iden id) (Typ,typ) g *)
       let t = (match find x g with
         | (k,TSlice t) -> t
         | _ -> raise (TypeError ("\"" ^ x ^ "\" must have type slice")))
       in
       let (_,(_,typ)) as te = tExpr g e in
       if t = typ then (Append(id,te), (pos, TSlice t))
       else raise (TypeError ("Mismatch in slice between \"" ^ typ_to_str t ^ "\" and \"" ^ typ_to_str typ))

    | Iden((i,_) as id) -> 
       let (_,t) = find i g in
       (Iden(id), (pos, t))

    | AValue(r,e) ->
       let (_,(_,typ1)) as tr = tExpr g r in
       let (_,(_,typ2)) as te = tExpr g e in
       (* do we allow e to be empty if this is a slice?? *)
       (match typ2 with
         | _ -> raise (TypeError "Array index must have type int")
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
            in (SValue(tr,id), (pos, field_typ))
         | _ -> raise (TypeError "Something very wrong")
       with 
         | Not_found -> raise (TypeError "Invalid struct field")
  in


  let rec tStmt frt g (p, pos) : Typed.annotated_utstmt = match p with 
    | Assign(xs,es) -> 
       let txs = List.map (tExpr g) xs in
       let tes = List.map (tExpr g) es in
       List.iter
         (fun ((_,(_,tx)),(_,(_,ty))) ->
           if not (tx == ty)
           then raise (TypeError "Type mismath in assign"))
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
       then raise (TypeError "If condition must have typ bool")
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
         
    | Var_stmt(ids_eso_typo_ls) ->
       let t_ids_eso_typo_ls =
         List.map
           (fun (a,eso,c) -> 
             let eso = match eso with
                        | Some(eso) -> Some(List.map (tExpr g) eso)
                        | None -> None
             in
             (a,eso ,c))
           ids_eso_typo_ls
       in
       (Var_stmt(t_ids_eso_typo_ls), pos)

    | SDecl_stmt(ids, None) ->
       (SDecl_stmt(ids, None), pos)
       
    | SDecl_stmt(ids, Some(exps)) ->
       let teso = (List.map (tExpr g) exps) in
       (SDecl_stmt(ids, Some(teso)), pos)

    | Type_stmt(id_typ_ls) -> 
       (Type_stmt(id_typ_ls),pos)
 
    | Expr_stmt e ->
       let te = tExpr g e in
       (Expr_stmt(te),pos)

    | Return(None) ->
       if not (frt == Void)
       then raise (DeclError "Function should return a value")
       else (Return(None),pos)
    | Return(Some(e)) ->
       let (_,(_,typ)) as te = tExpr g e in
       if frt == typ
       then (Return(Some(te)), pos)
       else raise (DeclError "Unexpected return type")

    | Break -> (Break, pos)
    | Block(s) -> 
       let ts = List.map (tStmt frt (scope g)) s in
       let out = (Block(ts), pos) in
       out;
    | Continue -> (Continue, pos)
    | Empty_stmt -> (Empty_stmt,pos)
    | _ -> (Continue, pos)

  in


  let rec tDecl g (d,pos) : Typed.annotated_utdecl = match d with
    | Var_decl(ids_eso_typo_ls) -> 
       let varDecl g (ids,eso,typo) : (string list * t_expr list option * typ option) * context =
         (* if (List.exists (id -> in_scope id g) ids) then raise DeclError "ID already declared in scope" (\* say which id? *\) *)
         (List.iter (fun id -> if in_scope id g
                               then raise (DeclError ("Variable \""^id^"\" already declared in scope"))
                               else ()) ids);
         match (eso,typo) with
         | (None, None) -> raise (DeclError ("Variables declaration must have either type or expressions")) (*right?*)
         (* | (None, Some typ) -> let g1 = snd (thread (fun id gam -> (id, add id typ gam)) g ids) in ((ids,eso,typo),g1) *)
         | (None, Some typ) -> List.iter (fun id -> add id (Var, typ) g) ids; ((ids,None,typo),g)
         (* do all the es have to have the same type? *)
         | (Some es, None) ->
            let pairs = zip ids es in
            let tes = List.map
                        (fun (id,e) -> let te = tExpr g e in add id (Var, te.typ) g; te)
                        pairs in
            ((ids, Some tes, typo), g)
         | (Some es, Some typ) ->
            let pairs = zip ids es in
            let tes = List.map (fun (id,e) ->
                             let te = tExpr g e in
                             if te.typ = typ then add id (Var, typ) g
                             else raise (DeclError ("Type mismatch on \""^id^"\"'s matching expression")); te) pairs in
            ((ids, Some tes, typo),g)

            (* let tes = List.map typ es g in *)
            (*                       if List.forall (fun te -> te.typ = typ) tes *)
            (*                       then thread (fun id -> add te.typ = typ) g ids *)
       in let (ls,g) = thread varDecl g ids_eso_typo_ls in
          (Var_decl(ls),g)
       (* if Ctx.in_scope typ_id gamma then raise DeclError "Already declared in scope" *)
       (* else Ctx.add typ_id gamma *)

    | Type_decl(typId_typ_ls) -> 
       (List.iter (fun (num,typ) -> if in_scope num g
                                    then raise (DeclError ("Type \""^num^"\" already declared in scope"))
                                    else add num (Typ, typ) g)
                  typId_typ_ls);
       (Type_decl(typId_typ_ls), g)
    | Func_decl(fId, id_typ_ls, typ, ps) -> (* start by creating a new frame?? *)
       if in_scope fId g
       then raise (DeclError ("Function \""^fId^"\" already declared"))
       else add fId (Fun, typ) g; List.iter (fun (id,typ) -> add id (Var, typ) g) id_typ_ls;
       let (tps,g1) = thread (tStmt typ) g ps in
       (Func_decl(fId, id_typ_ls, typ, tps), g)

  and tDecls gamma ds = thread tDecl gamma ds
  in
  (* fst is the typed tree, snd is the final context *)
  TProg(pkg, fst (tDecls (init()) decls))

(* lvalues or strings in the  context? *)

(* (build_symtab decls stdout false) *)
(* why outc? *)

(* build_symtab (Prog(_,decls)) outc dumpsymtab =  *)

(* replace Ctx.empty with a context that already has bool, int, float, ... *)
(*
Types:
bool byte complex64 complex128 error float32 float64
int int8 int16 int32 int64 rune string
uint uint8 uint16 uint32 uint64 uintptr
Constants:
true false iota
Zero value: nil
Functions:
append cap close complex copy delete imag len make new panic print println real recover
*)

  (* let rec tTyp gamma = function *)
  (*   | TSimp(typ_id) -> failwith "not implemented" *)
  (*      (\* if Ctx.mem typ_id gamma then  *\) *)
  (*      (\* if Ctx.in_scope typ_id gamma then raise DeclError "Already declared in scope" *\) *)
  (*      (\* else Ctx.add typ_id gamma *\) *)
  (*   | TStruct(x_typ_ls) -> failwith "not implemented" *)
  (*   | TArray(typ,d) -> failwith "not implemented" *)
  (*   | TSlice(typ) -> failwith "not implemented" *)
  (*   | Void -> failwith "not implemented" *)
  (* in *)

    (* | ILit(d) -> ILit(d) *)
    (* | FLit(f) -> FLit(f) *)
    (* | BLit(b) -> BLit(b) *)
    (* | RLit(c) -> RLit(c) *)
    (* | SLit(s) -> SLit(s) *)

       (* let t = (match (te.typ, op) with *)
       (*          (\* just to start with *\) *)
       (*          | TSimp "int", Positive -> TSimp "int" *)
       (*          | TSimp "float64", Positive -> TSimp "float64" *)
       (*          | TSimp "rune", Positive -> TSimp "rune" *)
       (*          | TSimp "int", Negative -> TSimp "int" *)
       (*          | TSimp "float64", Negative -> TSimp "float64" *)
       (*          | TSimp "rune", Negative -> TSimp "rune" *)
       (*          | TSimp "bool", Boolnot -> TSimp "bool" *)
       (*          | TSimp "int", Bitnot -> TSimp "int" *)
       (*          | TSimp "rune", Bitnot -> TSimp "rune" *)
       (*          | _ -> raise (TypeError ("Mismatch with '" ^ uop_to_str op ^ "' operation"))) *)
       (*           (\* change to allow for new types *\) *)
