(* module Ctx = Map.Make(String) *)

open Ast
open Context
exception TypeError of string
exception DeclError of string

let str_of_lv = function
  | Iden(id) -> id
  | AValue(t_lvalue, t_expr) -> failwith "not done"
  | SValue(t_lvalue, id) -> failwith "not done"

(* type context = lvalue Ast.Ctx.t *)

(* type context = string Ast.Ctx.t *)

let mapo f o = match o with
  | None -> None
  | Some x -> Some (f x)

let typo f gam o = match o with
  | None -> (None, gam)
  | Some x -> let (tx,g) = f gam x in (Some(tx), g)

let rec list_type = function
  | [] -> failwith "empty list"
  | [x] -> x
  | x::y::tl -> if x = y then list_type (y::tl)
                else raise (TypeError ("Multiple types in single assignment"))

let rec zip l1 l2 = match (l1,l2) with
  | (x::xs, y::ys) -> (x,y)::zip xs ys
  | ([],[]) -> []
  | _ -> failwith "Mismatch on number of arguments"

let rec unzip l = match l with
  | (x,y)::tl -> let (xs,ys) = unzip tl in (x::xs,y::ys)
  | [] -> ([],[])

let typeAST (Prog(pkg,decls)) =
  let rec thread f gamma = function (* map, but updated gamma is used for next element *)
    | [] -> ([],gamma)
    | x::xs -> let (d,g) = f gamma x in
               let (tl,gam) = thread f g xs in
               (d::tl,gam)
  in
  (* let rec tExpr gamma = function *)
  let rec tExpr g e : t_expr = match e with
    | Lvalue(l) -> failwith "not implemented"
       (* let tl = tLVal g l in *)
       (* { exp = Lvalue tl.exp ; typ = tl.typ } *)

    | ILit(d) -> { exp = ILit d ; typ = TSimp "int" }
    | FLit(f) -> { exp = FLit f ; typ = TSimp "float" }
    | BLit(b) -> { exp = BLit b ; typ = TSimp "bool" }
    | RLit(c) -> { exp = RLit c ; typ = TSimp "rune" }
    | SLit(s) -> { exp = SLit s ; typ = TSimp "string" }
    | Bexp(op,e1,e2) -> 
       let te1 = tExpr g e1 in
       let te2 = tExpr g e2 in
       let t = (match (te1.typ, te2.typ, op) with
                (* just to start with *)
                | TSimp "int", TSimp "int", Plus -> TSimp "int"
                | _ -> failwith "operation not handled")
       in { exp = Bexp(op,te1,te2) ; typ = t }
                 
       (* let te1 = typeExpr gamma e1 in  *)
       (* let te2 = typeExpr gamma e2 in  *)
       (* let t = (match (te1.typ, te2.typ, op) with *)
       (*          | TInt, TInt, _ -> TInt *)
       (*          | TInt, TFloat, _ -> TFloat *)
       (*          | TFloat, TInt, _ -> TFloat *)
       (*          | TFloat, TFloat, _ -> TFloat *)
       (*          | TString, TString, PLUS -> TString *)
       (*          | TString, TString, MINUS -> TString *)
       (*          | _ -> raise (TypeError ("Mismatch with '" ^ str_of_binop op ^ "' operation"))) *)
       (* in { exp = Bexp(op,te1,te2) ; typ = t } *)
    | Uexp(op,e) -> 
       let te = tExpr g e in
       let t = (match (te.typ, op) with
                (* just to start with *)
                | TSimp "int", _ -> TSimp "int"
                | _ -> failwith "operation not handled")
       (* let te = typeExpr gamma e *)
       in { exp = Uexp(op,te) ; typ = t }
    | Fn_call(fun_id, es) -> failwith "not implemented"
    | Append(x, e) -> failwith "not implemented"
  and tLVal g l : t_lvalue = match l with
    | Iden(id) -> failwith "not implemented"
    | AValue(r,e) -> failwith "not implemented"
    | SValue(r,id) -> failwith "not implemented"

  in
  let get_assign_typ g (lv,e) = 
    let t_lv = tLVal g lv in
    (* fix t_lv problems, currently, it just compares with the string for Id and rejects for other lvalues *)
    let id = str_of_lv lv in
    (* if not (mem id g) *)
    if not (in_scope id g)
    then raise (DeclError("Assignment of undeclared variable \""^id^"\""))
    else let tid = find id g in
         let te = tExpr g e in
         if (tid = te.typ) (*|| (tid = TFloat && te.typ = TInt) add parametricity!*)
         then (t_lv,te)
         else raise (TypeError "Mismatch in assignment")
  in
  (* let rec tStmt gamma = function *)
  let rec tStmt g p : t_stmt * context = match p with
      (* DOES NOT CHECK THAT TYPES HAVE THE RIGHT FORM!! *)
    | Assign(xs,es) -> 
       let (txs, tes) = unzip (List.map (get_assign_typ g) (zip xs es)) in
       (Assign(txs, tes), g)

    | Print(es) -> (Print(List.map (tExpr g) es), g) (* change tExpr to return a pair and use thread instead of map? *)
    | Println(es) -> (Println(List.map (tExpr g) es), g)
    | If_stmt(po,e,ps,pso) ->
       let (tpo,g1) = typo tStmt g po in
       let te = tExpr g1 e in
       let (tps,g2) = thread tStmt g1 ps in
       let (tpso,g3) = typo (thread tStmt) g2 pso in
       (* (If_stmt(tpo, te, tps, mapo (thread tStmt g) pso), g) *)
       (If_stmt(tpo, te, tps, tpso), g3)

    | Switch_stmt(po, eo, ps) -> 
       let (tpo,g1) = typo tStmt g po in
       let teo = mapo (tExpr g1) eo in
       let (tps,g2) = thread tStmt g1 ps in
       (Switch_stmt(tpo, teo, tps), g2)

    | Switch_clause(eso, ps) ->
       let teso = mapo (List.map (tExpr g)) eso in
       let (tps,g1) = thread tStmt g ps in
       (Switch_clause(teso, tps), g1)

    | For_stmt(po1, eo, po2, ps) ->
       let (tpo1,g1) = typo tStmt g po1 in
       let teo = mapo (tExpr g1) eo in
       let (tpo2,g2) = typo tStmt g1 po2 in
       let (tps,g3) = thread tStmt g2 ps in
       (For_stmt(tpo1, teo, tpo2, tps), g3)
         
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
       (Return(teo), g)
    | Break -> (Break, g)
    | Block(s) -> 
       let (ts,g1) = thread tStmt g s in
       (Block(ts), g1)
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
         | (None, Some typ) -> List.iter (fun id -> add id typ g) ids; ((ids,None,typo),g)
         (* do all the es have to have the same type? *)
         | (Some es, None) ->
            let pairs = zip ids es in
            let tes = List.map
                        (fun (id,e) -> let te = tExpr g e in add id (te.typ) g; te)
                        pairs in
            ((ids, Some tes, typo), g)
         | (Some es, Some typ) ->
            let pairs = zip ids es in
            let tes = List.map (fun (id,e) ->
                             let te = tExpr g e in
                             if te.typ = typ then add id typ g
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
                             else ()) typId_typ_ls);
       (List.iter (fun (num,typ) -> add num typ g) typId_typ_ls); (* should we use a different kind of add? *)
       (Type_decl(typId_typ_ls), g)
    | Func_decl(fId, id_typ_ls, typ, ps) -> 
       let (tps,g1) = thread tStmt g ps in
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
