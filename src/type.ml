(* module Ctx = Map.Make(String) *)
open Ast
open Context
open AuxFunctions
open Errors

let dumpsymtab = true (* for testing purposes *)

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


let typeAST (Prog(pkg,decls)) =
  let rec thread f gamma = function (* map, but updated gamma is used for next element *)
    | [] -> ([],gamma)
    | x::xs -> let (d,g) = f gamma x in
               let (tl,gam) = thread f g xs in
               (d::tl,gam)
  in
  (* let rec tExpr gamma = function *)
  let rec tExpr g e : t_expr = match e with
    | Lvalue(l) ->
       let (tl,t) = tLVal g l in
       { exp = Lvalue(tl) ; typ = t }

    | ILit(d) -> { exp = ILit d ; typ = TSimp "int" }
    | FLit(f) -> { exp = FLit f ; typ = TSimp "float64" }
    (* | BLit(b) -> { exp = BLit b ; typ = TSimp "bool" } *) (* why is bool not included in the pdf?? *)
    | RLit(c) -> { exp = RLit c ; typ = TSimp "rune" }
    | SLit(s) -> { exp = SLit s ; typ = TSimp "string" }
    | Bexp(op,e1,e2) -> 
       let te1 = tExpr g e1 in
       let te2 = tExpr g e2 in
       let lub = unify g te1.typ te2.typ in
       let t = (match op with
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

                (* | TSimp "int", TSimp "int", Plus -> TSimp "int" *)
                (* | TSimp "int", TSimp "int", Plus -> TSimp "int") *)

       (* in { exp = Bexp(op,te1,te2) ; typ = t } *)
       in { exp = Bexp(op,te1,te2) ; typ = t }

    | Uexp(op,e) -> 
       let te = tExpr g e in
       let base = getBaseTyp g te.typ in
       let t = (match op with
                | Positive	when isNumeric base -> base
                | Negative	when isNumeric base -> base
                | BoolNot	when isBool base -> base
                | BitNot	when isInteger base -> base
                | _ -> raise (TypeError ("Mismatch with '" ^ uop_to_str op ^ "' operation")))
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
       (* let te = typeExpr gamma e *)
       in { exp = Uexp(op,te) ; typ = t }
    | Fn_call(f,es) -> 
       let tf = tLVal g f in
       let (fargs,ft) = ffind f g in
       let tes = List.map (tExpr g) es in
       List.iter (fun (arg,te) ->
           if arg != te.typ
           then raise (TypeError ("Function argument mistmatch between "^typ_to_str arg^" and "^typ_to_str te.typ))
           else ()) (zip fargs tes);
       { exp = Fn_call(tf,tes) ; typ = ft ; }
    (* why does the pdf say that the arguments have to be well typed (they're lvals, ids, or something else?? *)
    | Append(x,e) ->
       let t = (match tfind g x with
(* add (Iden id) (Typ,typ) g *)
         | TSlice t -> t
         | _ -> raise (TypeError ("\"" ^ x ^ "\" must have type slice")))
       in
       let te = tExpr g e in
       if t = te.typ then { exp = Append(x,te) ; typ = TSlice t }
       else raise (TypeError ("Mismatch in slice between \"" ^ typ_to_str t ^ "\" and \"" ^ typ_to_str te.typ))
  (* missing typecast? *)
  (* and tLVal g l : t_lvalue * typ = match l with *)
  and tLVal g l : t_lvalue = match l with
    | Iden(id) -> (Iden(id),find id g)
    | AValue(r,e) ->
       let tr = tLVal g r in
       let te = tExpr g e in
       (* do we allow e to be empty if this is a slice?? *)
       if te.typ = TSimp "int"
       then raise (TypeError "Array index must have type int")
       (* else { lval = AValue(tr,te) ; typ = TArray (te.typ,te.exp) } *)
       else failwith "not yet"
    (* make tlval a record and return tr.typ? *)
    | SValue(r,id) -> 
       let tr = tLVal g r in
       let id_typ_ls = tr.typ in
       if tr.typ = TSimp "int"
       then raise (TypeError "Struct type error")
       (* else { lval = SValue(tr,id) ; typ = TStruct(tr.typ,0) } *)
       else failwith "not yet"
  in
  let get_assign_typ g (lv,e) = 
    let t_lv = tLVal g lv in
    (* fix t_lv problems, currently, it just compares with the string for Id and rejects for other lvalues *)
    let id = lv_to_str lv in
    (* if not (mem id g) *)
    if not (in_scope id g)
    then raise (DeclError("Assignment of undeclared variable \""^id^"\""))
    else let (_, tid) = find id g in
         let te = tExpr g e in
         if (tid = te.typ) (*|| (tid = TFloat && te.typ = TInt) add parametricity!*)
         then (t_lv,te)
         else raise (TypeError "Mismatch in assignment")
  in
  (* let rec tStmt gamma = function *)
  let rec tStmt frt g p : t_stmt * context = match p with (* frt is function return type *)
    (* Should assign take lvalues?? *)
    | Assign(xs,es) -> 
       let (txs, tes) = unzip (List.map (get_assign_typ g) (zip xs es)) in
       (Assign(txs, tes), g)
    (* are we missing op assignment?? *)

    | Print(es) -> (Print(List.map (tExpr g) es), g) (* change tExpr to return a pair and use thread instead of map? *)
    (* why do the notes say that println takes an expression, not a list? *)
    | Println(es) -> (Println(List.map (tExpr g) es), g)
    (* why do the notes say that println takes two expressions? *)
    | If_stmt(po,e,ps,pso) ->
       let (tpo,g1) = typo (tStmt frt) g po in
       let te = tExpr g1 e in
       if te.typ != TSimp "bool"
       then raise (TypeError "If statement must have typ bool")
       else
         let (tps,g2) = thread (tStmt frt) g1 ps in
         let (tpso,g3) = typo (thread (tStmt frt)) g2 pso in
         (* (If_stmt(tpo, te, tps, mapo (thread (tStmt frt) g) pso), g) *)
         (If_stmt(tpo, te, tps, tpso), g3)

    (* how do switch clause and switch statement compare to what's presented in typecheck.pdf?? *)
    | Switch_stmt(po, eo, ps) -> 
       let (tpo,g1) = typo (tStmt frt) g po in
       let teo = mapo (tExpr g1) eo in
       let (tps,g2) = thread (tStmt frt) g1 ps in
       (Switch_stmt(tpo, teo, tps), g2)
    | Switch_clause(eso, ps) ->
       let teso = mapo (List.map (tExpr g)) eso in
       let (tps,g1) = thread (tStmt frt) g ps in
       (Switch_clause(teso, tps), g1)

    | For_stmt(po1, eo, po2, ps) -> 
       (* should I remove threading?? replace the hash table with a map? *)
       let (tpo1,g1) = typo (tStmt frt) g po1 in
       let teo = mapo (tExpr g1) eo in
       let (tpo2,g2) = typo (tStmt frt) (scope g1) po2 in
       let (tps,g3) = thread (tStmt frt) g2 ps in
       (For_stmt(tpo1, teo, tpo2, tps), (unscope stdout dumpsymtab g3))
         
    | Var_stmt(ids_eso_typo_ls) ->
       let t_ids_eso_typo_ls =
         List.map (fun (a,eso,c) -> (a,mapo (List.map (tExpr g)) eso,c)) ids_eso_typo_ls in
       (Var_stmt(t_ids_eso_typo_ls), g)

    | SDecl_stmt(ids, eso) ->
       (* should this be lvalues instead of ids? *)
       let teso = mapo (List.map (tExpr g)) eso in
       (SDecl_stmt(ids, teso), g)

    | Type_stmt(id_typ_ls) -> (Type_stmt(id_typ_ls),g)
    | Expr_stmt e ->
       let te = tExpr g e in
       (Expr_stmt(te), g)
    | Return(eo) ->
       let teo = mapo (tExpr g) eo in
       (match teo with
       | None -> (Return(teo), g)
       | Some te -> 
          if frt = te.typ then (Return(teo), g)
          else raise (DeclError (typ_to_str te.typ^" does not match expected return type "^typ_to_str te.typ)))
    | Break -> (Break, g)
    | Block(s) -> 
       let (ts,g1) = thread (tStmt frt) (scope g) s in
       (Block(ts), (unscope stdout dumpsymtab g1))
    | Continue -> (Continue, g)
    | Empty_stmt -> (Empty_stmt, g)

(* and tStmts xs = thread tStmt g s in *)

(*
  let rec typeStmt = function
    | Read(id) -> if Ctx.mem id gamma
                  then Read(id)
                  else raise (DeclError ("Read of undeclared variable \""^id^"\""))
    | Ifte(e,xs,ys) ->
       let te = typeExpr gamma e in
       if te.typ = TInt
       then Ifte(typeExpr gamma e, typeStmts xs, typeStmts ys)
       else raise (TypeError "Expression in if-then-else statement must have int type")
 *)

  in
  (* let rec tDecl gamma = function *)
  (* let varDecl gamma (ids,eso,typo) = *)
    
  let rec tDecl g d : t_decl * context = match d with
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
