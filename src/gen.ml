open Ast
open Context
open Ho 
open AuxFunctions
open Printf
exception GenError of string

let generate table (Prog(id,decls) : Typed.ast) oc =
  (* may need memTable when implementing strings *)
  (*let memTable : (int, string) Hashtbl.t = Hashtbl.create 1337 in*) (* addr loc, value *)
  let globalVar : (string, (string * int)) Hashtbl.t = Hashtbl.create 1337 in (* var name, (type, addr loc) *)
  let globc = ref 0 in (* function count for global variables *)
  let segc = ref 4 in (* segment count *)
  let tabc = ref 0 in (* tab count *)
  let switchTag = ref None in (* expr switch *)
  let mainFunc = ref false in (* expr switch *)
  (* let pln() = fprintf oc "\n" in (* print line *) *)
  let pstr s = fprintf oc "%s" s in (* print ocaml string *)
  (* let pid id = pstr (fst id) in *)
  let rec tabWith n = if n > 0 then (pstr "  "; tabWith (n-1)) in
  let tab() = tabWith !tabc in
  let psfl s f = (* print string followed list *)
    List.iter (fun y -> f y; pstr s)
  in
  let pssl s f = function (* print string separated list *)
    | [] -> ()
    | x::xs -> f x; List.iter (fun y -> pstr s; f y) xs
  in
  let pcsl = pssl ", " in
  let plsl f = function (* print comma separated list *)
    | [] -> ()
    | x::xs -> f x; List.iter (fun y -> pstr "\n"; f y) xs
  in
  let rec type_name (at:Typed.uttyp) = match at with
    | TSimp("bool", _)    -> "bool"
    | TSimp("int", _)     -> "int"
    | TSimp("float64", _) -> "float64"
    | TSimp("rune", _)    -> "rune"
    | TSimp("string", _)  -> "string"
    | TStruct(a)  -> sprintf "struct{%s}"
                        (match a with
                          | (i,t)::xs ->
                            (type_name t) ^ (List.fold_left (fun acc (i,t) -> acc^","^(type_name t)) "" xs)
                          | _ -> "")
    | TArray(t,l) -> sprintf "[%d]%s" l (type_name t)
    | TSlice(t)   -> sprintf "[]%s" (type_name t)
    | _           -> failwith "Unsupported type"
  in
 

  let rec gTyp (at:Typed.uttyp) = match at with
    (* do we need to check if the base names have been aliased?? *)
    | TSimp("bool", _)    -> "i32"
    | TSimp("int", _)     -> "i32"
    | TSimp("rune", _)    -> "i32"
    | TSimp("string", _)  -> "i32"
    | TSimp("float64", _) -> "f64"
    (* I'm not sure about this: *)
    | TSimp(name,ctx) -> (match find name ctx with
                              | None -> raise (GenError "Base type not found")
                              | Some e -> gTyp e)
    | TStruct(a)  -> "i32"
    | TArray(a,b) -> "i32"
    | TSlice(a)   -> "i32"
    | TFn(a,b)    -> ""
    | TVoid       -> ""
    | TKind(a)    -> gTyp a
  in
(*
  let rec gLhs e = match e with
    | Iden(id) ->
      let depth = scope_depth (get_scope id ctx) in
        if depth > 1 then
          fprintf oc "(set_local $%t %t)"
          (fun c -> pstr (getId v))
                                            (fun c -> gExpr e)
                                        else begin
                                          let styp = ref "" in
                                          let size = ref 0 in
                                          (match typ with
                                            | TSimp("float64", _) -> styp := "f64"; size := 8;
                                            | TSimp("int", _)
                                            | TSimp("rune", _)
                                            | TSimp("string", _)
                                            | TSimp("bool", _) ->    styp := "i32"; size := 4;
                                            | _ -> failwith "not implemented");
                                          fprintf oc "(%s.store (i32.const %d) %t)"
                                                      !styp
                                                      !segc
                                                      (fun c -> gExpr e);
                                          Hashtbl.add globalVar id (!styp, !segc);
                                          segc := !segc + !size;
                                        end
                          | _ -> failwith "Found non id in lhs of assignment"))
    | AValue() ->
    | SValue() ->
  in
*)
  let gBOp op t =
    let s = match op, t with
(*
    | Lt,        TStruct("string",_) -> "call $#string_lt"
    | Lteq,      TS("string",_)      -> "call $#string_le"
    | Gt,        TSimp("string",_)   -> "call $#string_gt"
    | Gteq,      TSimp("string",_)   -> "call $#string_ge"
*)
    | Equals,    TSimp("string",_) -> "call $#string_eq"
    | Notequals, TSimp("string",_) -> "call $#string_ne"
    | Lt,        TSimp("string",_) -> "call $#string_lt"
    | Lteq,      TSimp("string",_) -> "call $#string_le"
    | Gt,        TSimp("string",_) -> "call $#string_gt"
    | Gteq,      TSimp("string",_) -> "call $#string_ge"
    | Plus,      TSimp("string",_) -> "call $#string_cat"

    | Equals,t    -> (gTyp t) ^ ".eq"
    | Notequals,t -> (gTyp t) ^ ".ne"
    | Lt,t        -> (gTyp t) ^ ".lt_s"
    | Lteq,t      -> (gTyp t) ^ ".le_s"
    | Gt,t        -> (gTyp t) ^ ".gt_s"
    | Gteq,t      -> (gTyp t) ^ ".ge_s"
    | Plus,t      -> (gTyp t) ^ ".add"
    | Minus,t     -> (gTyp t) ^ ".sub"
    | Bitor,t     -> (gTyp t) ^ ".or"
    | Bitand,t    -> (gTyp t) ^ ".and"
    | Bitxor,t    -> (gTyp t) ^ ".xor"
    | Times,t     -> (gTyp t) ^ ".mul"
    | Div,t       -> (gTyp t) ^ ".div_s"
    | Modulo,t    -> (gTyp t) ^ ".rem_s"
    | Lshift,t    -> (gTyp t) ^ ".shl_s"
    | Rshift,t    -> (gTyp t) ^ ".shr_s"
    | Boolor,t    -> (gTyp t) ^ ".and"
    | Booland,t   -> (gTyp t) ^ ".or"
    | Bitnand,t   -> (gTyp t) ^ ".shr_s"
    in pstr s
  in
  let hsh s = "g#"^s in
  let rec addHash ((ue,(pos,typ,ctx)):Typed.annotated_texpr) =
    let hue = match ue with
    | Iden(id)     -> Iden(hsh id)
    | AValue(r,e)  -> AValue(addHash r,addHash e)
    | SValue(r,id) -> SValue(addHash r,hsh id)
    | ILit(_) 
    | FLit(_)
    | RLit(_)
    | SLit(_)            -> ue
    | Bexp(op,e1,e2)     -> Bexp(op,addHash e1,addHash e2)
    | Uexp(op,e)         -> Uexp(op,addHash e)
    | Fn_call(fun_id,es) -> Fn_call(fun_id,List.map addHash es)
    | Append(x,e)        -> Append(hsh x,addHash e)
    in (hue,(pos, typ ,ctx))
  in
  let rec size_of_type t = match t with
    | TSimp("bool", _)
    | TSimp("rune", _)
    | TSimp("int", _)
    | TSimp("string", _) -> 4
    | TStruct(tl) -> List.fold_left (fun acc (i,t) -> (size_of_type t) + acc) 0 tl
    | TArray(t, l) -> (size_of_type t) * l
    | TSlice(_) -> 4
    | TSimp("float64", _) -> 8
    | _ -> 90
  in
  let complex_type t = match t with
    | TSimp("bool", _)
    | TSimp("rune", _)
    | TSimp("int", _)
    | TSimp("float64", _)
    | TSimp("string", _) -> false
    | _ -> true
  in
(*
  let rec gen_copy_value
    | TSimp("bool", _)
    | TSimp("rune", _)
    | TSimp("int", _)
    | TSlice(_)
    | TSimp("string", _) -> 4
    | TSimp("float64", _) -> 8
    | TStruct(_)
    | TArray(t, l) -> 0
    | _ -> failwith "impossible in gen_copy_value"
  in
*)
  let rec alphaRenaming id d (at:Typed.uttyp) : string = match at with
    | TFn(_,_)
    | TVoid
    | TKind(_) -> failwith "kinds not yet supported"
    | _ -> sprintf "%s_%s_%d " id (type_name at) d
(* type:   ( type <var> ) *)
(* type:    ( type <name>? ( func <param>* <result>? ) ) *)
  in
  let rec gExpr ((ue,(pos,typ,ctx)):Typed.annotated_texpr) doLoad=
    match ue with
    | Iden(id) ->
      if ((id = "true") || (id = "false")) && (0 = (scope_depth (get_scope id ctx)))
      then match id with
        | "true"  -> fprintf oc "(i32.const 1)"
        | "false" -> fprintf oc "(i32.const 0)"
        | _ -> failwith "Strange things happenning in wonderland"
      else
      let depth = scope_depth (get_scope id ctx) in
        if depth > 1 then
          fprintf oc "(get_local $%t)"
                     (fun c -> pstr (alphaRenaming id depth typ))
        else begin
          let global = Hashtbl.find globalVar id in
          if not (complex_type typ) && doLoad
          then fprintf oc "(%s.load (i32.const %d))" (fst global) (snd global)
          else fprintf oc "(i32.const %d)" (snd global) 
        end

    | AValue(((_,(_,TSlice(t),_)) as e1),e2)
    | AValue(((_,(_,TArray(t,_),_)) as e1),e2) ->
       if not (complex_type typ) && doLoad
       then fprintf oc "(%s.load (i32.add %t (i32.mul (i32.const %d) %t)))"
           (gTyp typ)
           (fun c -> gExpr e1 false)
           (size_of_type t)
           (fun c -> gExpr e2 true)
       
       else fprintf oc "(i32.add %t (i32.mul (i32.const %d) %t))"
           (fun c -> gExpr e1 false)
           (size_of_type t)
           (fun c -> gExpr e2 true)
       
    | AValue(_,_) -> failwith "Typechecking leaks invalid array references"
                                                        
    | SValue(((_,(_,TStruct(l),_)) as r),id) ->
      let rec field_offset id l acc = match l with
        | (i,t)::xs when i = id -> acc
        | (i,t)::xs -> field_offset id xs (acc + (size_of_type t))
        | _ -> failwith "Typechecking doesn't check struct fields correctly"
      in
       if (not (complex_type typ)) && doLoad
       then fprintf oc "(%s.load (i32.add %t (i32.const %d)))"
               (gTyp typ)
               (fun c -> gExpr r true)
               (field_offset id l 0)
       else fprintf oc "(i32.add %t (i32.const %d))"
               (fun c -> gExpr r true)
               (field_offset id l 0)
                        
    | SValue(r,id) -> failwith "Unexpected type in SValue"
                        
    (* | Parens(e)  -> fprintf oc "(%t)" (fun c -> gExpr e) *)
    | ILit(d) -> fprintf oc "(i32.const %d)" d
    | FLit(f) -> fprintf oc "(f64.const %f)" f
    | RLit(c) -> fprintf oc "(i32.const %d)" (int_of_char c)
    | SLit(s) -> fprintf oc
                  ("(block (i32.store (i32.load (i32.const 0)) (i32.const %d))"
                     ^^ " %t "
                     ^^ " (i32.store (i32.const 0) (i32.add (i32.const %d) (i32.load (i32.const 0))))"
                     ^^ " (i32.sub (i32.load (i32.const 0)) (i32.const %d)))")
                  (String.length s)
                  (fun c -> String.iteri
                             (fun i c -> (fprintf oc "\n(i32.store (i32.add (i32.load (i32.const 0)) (i32.const %d)) (i32.const %d))" ((i+1) * 4) (Char.code c)))
                             s)
                  (((String.length s) + 1) * 4)
                  (((String.length s) + 1) * 4)
       (* gExpr (Bexp(Bitand,1,(Bitor,e1,e2)),(pos,typ,ctx)) *)
       (* gExpr (Bexp(Bitand,1,(Bitand,e1,e2)),(pos,typ,ctx)) *)
       (* gExpr (Uexp(Bitnot,(Bitand,e1,e2)),(pos,typ,ctx)) *)

   | Bexp(Bitnand,e1,e2) -> 
       fprintf oc "(i32.neg (i32.const 1) (i32.and %t %t))"
               (fun c -> gExpr e1 true)
               (fun c -> gExpr e2 true)

    | Bexp(Notequals,((_,(_,(TStruct(_) as t), _)) as e1),e2)
    | Bexp(Notequals,((_,(_,(TArray(_) as t), _)) as e1),e2) ->
        fprintf oc "(call $#nequals_bytes %t %t (i32.const %d))"
                   (fun c -> gExpr e1 true)
                   (fun c -> gExpr e2 true)
                   ((size_of_type t) / 4)
    | Bexp(Equals,((_,(_,(TStruct(_) as t), _)) as e1),e2)
    | Bexp(Equals,((_,(_,(TArray(_) as t), _)) as e1),e2) ->
        fprintf oc "(call $#equals_bytes %t %t (i32.const %d))"
                   (fun c -> gExpr e1 true)
                   (fun c -> gExpr e2 true)
                   ((size_of_type t) / 4)
       
    | Bexp(op,((e,(p,t,c)) as e1),e2) ->
       fprintf oc "(%t %t %t)"
                      (fun c -> gBOp op t)
                      (fun c -> gExpr e1 true)
                      (fun c -> gExpr e2 true)
       
    (* | Uexp(Negative, ((_,(_,TSimp("int",_),_)) as e) ) ->  *)
    (* check the type? *)
    | Uexp(Negative, e) -> 
       fprintf oc "(i32.sub (i32.const 0) %t)"
                  (fun c -> gExpr e true)
    (* | Uexp(Boolnot, ((_,(_,TSimp("int",_),_)) as e) ) ->  *)
    | Uexp(Boolnot, e) -> 
       fprintf oc "(i32.and (i32.const 1) (i32.xor (i32.const 4294967295) %t))" (* 2^32 - 1 *)
                  (fun c -> gExpr e true)
    (* | Uexp(Bitnot, ((_,(_,TSimp("int",_),_)) as e) ) ->  *)
    | Uexp(Bitnot, e) -> 
       fprintf oc "(i32.xor (i32.const 4294967295) %t)"
                  (fun c -> gExpr e true)
    | Uexp(Positive,e) -> gExpr e true
       (* fprintf oc "(%t.%t %t)" *)
       (*                (fun c -> gTyp typ) *)
       (*                (fun c -> gUOp op) *)

    | Fn_call((Iden("float64"),_), [k]) -> 
       let (_,(_,t,_)) = k in
       (match sTyp2 t with
       | TSimp("float64",_) -> gExpr k true
       | _ -> fprintf oc "(f64.convert_s/i32 %t)" (fun c -> gExpr k true))

    | Fn_call((Iden("int"),_), [k])
    | Fn_call((Iden("bool"),_), [k])
    | Fn_call((Iden("rune"),_), [k]) -> 
       let (_,(_,t,_)) = k in
       (match sTyp2 t with
        | TSimp(("float64",_)) -> fprintf oc "(i32.trunc_s/f64 %t)" (fun c -> gExpr k true)
        | _ -> gExpr k true)

    | Fn_call((Iden(i),_), k) ->
      fprintf oc "(call $%t %t)"
        (fun c -> pstr i)
        (fun c -> pssl " "
                  (fun ((_,(_,t,_)) as x) ->
                     if complex_type t
                     then fprintf oc "(call $#copyarg %t (i32.const %d))"
                             (fun c -> gExpr x true)
                             (size_of_type t)
                     else gExpr x true)
                  k)
    | Fn_call(fun_id, es) -> failwith "fn_call without id"
        (* let i = get_name fun_id in *)
       (*                       fprintf oc "(call $%t %t)" *)
       (*                               (fun c -> pstr i) *)
       (*                               (fun c -> pssl " " gExpr k) *)
    | Append(x, e) -> failwith "appends not yet supported"
  in
  let rec getId (ue,(pos,typ,ctx):Typed.annotated_texpr):string =
    match ue with
    | Iden(id) -> let depth = scope_depth (get_scope id ctx) in
                  alphaRenaming id depth typ
    | _ -> failwith "Found non id in lhs of assignment"
  in
  let rec gAssign (((ue,_) as lhs),((_,(_,typ,ctx)) as rhs)) = match ue with
    | Iden(id) ->
      let depth = scope_depth (get_scope id ctx) in
      if depth > 1 then
        if complex_type typ
        then
        fprintf oc "(call $#copy (get_local %t) %t (i32.const %d))"
                   (fun c -> pstr (getId lhs))
                   (fun c -> gExpr rhs true)
                   (size_of_type typ)
        else
        fprintf oc "(set_local $%t %t)"
                   (fun c -> pstr (getId lhs))
                   (fun c -> gExpr rhs true)
      else begin
        let styp = gTyp typ in
        let size = size_of_type typ in
        if complex_type typ
        then
        fprintf oc "(call $#copy (i32.const %d) %t (i32.const %d))"
                   (snd (try (Hashtbl.find globalVar id) with
                          | _ -> failwith "Shouldn't happen"))
                   (fun c -> gExpr rhs true)
                   (size_of_type typ)
        else
        fprintf oc "(%s.store (i32.const %d) %t)"
                   styp
                   (snd (try (Hashtbl.find globalVar id) with
                          | _ -> failwith "Shouldn't happen"))
                   (fun c -> gExpr rhs true);
      end
    | AValue(e1,e2) ->
        if complex_type typ
        then
        fprintf oc "(call $#copy %t %t (i32.const %d))"
                   (fun c -> gExpr lhs false)
                   (fun c -> gExpr rhs true)
                   (size_of_type typ)
        else
        fprintf oc "(%s.store %t %t)"
                   (gTyp typ) 
                   (fun c -> gExpr lhs false)
                   (fun c -> gExpr rhs true)
        
    | SValue(e,i) ->
        if complex_type typ
        then
        fprintf oc "(call $#copy %t %t (i32.const %d))"
                   (fun c -> gExpr lhs false)
                   (fun c -> gExpr rhs true)
                   (size_of_type typ)
        else
        fprintf oc "(%s.store %t %t)"
                   (gTyp typ) 
                   (fun c -> gExpr lhs false)
                   (fun c -> gExpr rhs true)

    | _ -> failwith "Found non id in lhs of assignment"
  in
  let rec gStmt ((us, (pos,ctx)): Typed.annotated_utstmt) =
    match us with
    | Assign(xs, es) -> plsl gAssign  (zip xs es)
(*
       plsl (fun (v,e) -> let (ue,(_,typ,_)) = v in
                          (match ue with
                          | Iden(id) -> let depth = scope_depth (get_scope id ctx) in
                                        if depth > 0 then
                                          fprintf oc "(set_local $%t %t)"
                                            (fun c -> pstr (getId v))
                                            (fun c -> gExpr e)
                                        else begin
                                          let styp = ref "" in
                                          let size = ref 0 in
                                          (match typ with
                                            | TSimp("float64", _) -> styp := "f64"; size := 8;
                                            | TSimp("int", _)
                                            | TSimp("rune", _)
                                            | TSimp("string", _)
                                            | TSimp("bool", _) ->    styp := "i32"; size := 4;
                                            | _ -> failwith "not implemented");
                                          fprintf oc "(%s.store (i32.const %d) %t)"
                                                      !styp
                                                      !segc
                                                      (fun c -> gExpr e);
                                          Hashtbl.add globalVar id (!styp, !segc);
                                          segc := !segc + !size;
                                        end
                          | _ -> failwith "Found non id in lhs of assignment"))
                            *)
    | Var_stmt(xss) ->
       List.iter (plsl (fun (s,eo,typo) ->
        match (typo,eo) with
        | (Some typ,Some e) -> let depth = scope_depth (get_scope s ctx) in
                               fprintf oc "(set_local $%t %t)"
                                 (fun c -> pstr (alphaRenaming s depth typ))
                                 (fun c -> gExpr e true)
        | (None,Some e) -> let (_,(_,typ,_)) = e in
                           let depth = scope_depth (get_scope s ctx) in
                           fprintf oc "(set_local $%t %t)"
                             (fun c -> pstr (alphaRenaming s depth typ))
                             (fun c -> gExpr e true)

        | (Some typ,None) ->
          let depth = scope_depth (get_scope s ctx) in
          (match typ with
            | TSimp(_,_)  -> ()

            | TArray(t,l) ->
              let array_size = size_of_type typ in
              fprintf oc "(set_local $%t %t)"
                (fun c -> pstr (alphaRenaming s depth typ))
                (fun c -> fprintf oc ("(block (i32.store (i32.const 0) (i32.add (i32.load (i32.const 0)) (i32.const %d)))"
                                          ^^ "(i32.sub (i32.load (i32.const 0)) (i32.const %d)))")
                                  array_size
                                  array_size)
            | TStruct(l)  ->
              let struct_size = size_of_type typ in
              fprintf oc "(set_local $%t %t)"
                (fun c -> pstr (alphaRenaming s depth typ))
                (fun c -> fprintf oc ("(block (i32.store (i32.const 0) (i32.add (i32.load (i32.const 0)) (i32.const %d)))"
                                          ^^ "(i32.sub (i32.load (i32.const 0)) (i32.const %d)))")
                                  struct_size
                                  struct_size)
(*
            | TArray()    ->
*) 
            | _ -> failwith "Unsupported type 2")

        | _ -> failwith "weeding error"
       )) xss
       (* let ls = List.map (fun () -> ) (List.concat xss) in *)
       (* plsl (fun (v,e) -> fprintf oc "(set_local $%t %t)" *)
       (*                            (fun c -> pstr (getId v)) *)
       (*                            (fun c -> gExpr e)) ls *)

    | Print(es) ->
       let rec genprint es = match es with

           | ((_, (_,TSimp("bool",_),_)) as e)::xs -> 
               pstr "(call $#printbool ";
               gExpr e true;
               pstr ")";
               genprint xs
           | ((_, (_,TSimp("rune",_),_)) as e)::xs 
           | ((_, (_,TSimp("int",_),_)) as e)::xs -> 
               pstr "(call $#printi32 ";
               gExpr e true;
               pstr ")";
               genprint xs
           | ((_, (_,TSimp("float64",_),_)) as e)::xs -> 
               pstr "(call $#printf64";
               gExpr e true;
               pstr ")";
               genprint xs
           | ((_, (_,TSimp("string",_),_)) as e)::xs -> 
               pstr "(call $#printstring ";
               gExpr e true;
               pstr ")";
               genprint xs
           | _ -> () 
       in genprint es
       
    | Println(es) ->
       let rec genprint es = match es with
           | [(_, (_,TSimp("bool",_),_)) as e] -> 
               pstr "(call $#printlnbool ";
               gExpr e true;
               pstr ")"
           | [(_, (_,TSimp("rune",_),_)) as e] 
           | [(_, (_,TSimp("int",_),_)) as e] -> 
               pstr "(call $#printlni32 ";
               gExpr e true;
               pstr ")"
           | [(_, (_,TSimp("float64",_),_)) as e] -> 
               pstr "(call $#printlnf64";
               gExpr e true;
               pstr ")"
           | [(_, (_,TSimp("string",_),_)) as e] -> 
               pstr "(call $#printlnstring ";
               gExpr e true;
               pstr ")"

           | ((_, (_,TSimp("bool",_),_)) as e)::xs -> 
               pstr "(call $#printbool ";
               gExpr e true;
               pstr ")";
               pstr "(call $#writei32 (i32.const 32))";
               genprint xs
           | ((_, (_,TSimp("rune",_),_)) as e)::xs 
           | ((_, (_,TSimp("int",_),_)) as e)::xs -> 
               pstr "(call $#printi32 ";
               gExpr e true;
               pstr ")";
               pstr "(call $#writei32 (i32.const 32))";
               genprint xs
           | ((_, (_,TSimp("float64",_),_)) as e)::xs -> 
               pstr "(call $#printf64";
               gExpr e true;
               pstr ")";
               pstr "(call $#writei32 (i32.const 32))";
               genprint xs
           | ((_, (_,TSimp("string",_),_)) as e)::xs -> 
               pstr "(call $#printstring ";
               gExpr e true;
               pstr ")";
               pstr "(call $#writei32 (i32.const 32))";
               genprint xs
           | _ -> failwith "Println of unimplemented type"
       in 
       genprint es
        
    | If_stmt(po,e,ps,pso) ->
       may (fun s -> gStmt s; pstr "\n"; tab()) po;
       pstr "(if ";
       gExpr e true;
       pstr "\n";
       incr tabc;
       tab();
       pstr "(then\n";
       incr tabc;
       plsl (fun st -> tab(); gStmt st) ps;
       decr tabc;
       pstr ")";

       may (fun ps ->
             pstr "\n";
             tab();
             pstr "(else\n";
             incr tabc;
             plsl (fun st -> tab(); gStmt st) ps;
             decr tabc;
             pstr ")")
            pso;
       decr tabc;
       pstr ")\n"
    | Block(stmts) ->
       pstr "(block\n";
       incr tabc;
       plsl (fun st -> tab(); gStmt st) stmts;
       decr tabc;
       tab();
       pstr ")"
                   
  (* ( block <name>? <expr>* ) *)
    
    | Switch_stmt(po, eo, ps) ->
       switchTag := eo;
       let case = List.filter (fun s -> (match fst s with
                                         | Switch_clause(Some(e),_) -> true
                                         | _ -> false)) ps in
       let default = List.filter (fun s -> (match fst s with
                                            | Switch_clause(None,_) -> true
                                            | _ -> false)) ps in
       fprintf oc "%t(block $#break%t%t)\n"
                     (fun c -> may (fun p -> gStmt p; pstr "\n"; tab()) po)
                     (fun c -> incr tabc; List.iter gStmt case)
                     (fun c -> List.iter gStmt default);
       decr tabc
    | Switch_clause(eso, ps) ->
       (match eso with
        | None -> pstr "\n"; plsl (fun st -> tab(); gStmt st) ps
        | Some es -> List.iter (fun e ->
                       pstr "\n";
                       tab();
                       pstr "(if ";
                       (match !switchTag with
                        | None -> gExpr e true; pstr "\n"
                        | Some t -> pstr "(i32.eq ";
                                    gExpr t true;
                                    pstr " ";
                                    gExpr e true;
                                    pstr ")\n");
                       incr tabc;
                       tab();
                       pstr "(then\n";
                       incr tabc;
                       plsl (fun st -> tab(); gStmt st) ps;
                       pstr "\n";
                       tab();
                       pstr "(br $#break)))";
                       decr tabc; decr tabc) es)

    | For_stmt(po1, eo, po2, ps) ->
       may (fun s -> gStmt s; pstr "\n"; tab()) po1;
       pstr "(loop $#break $#continue\n";
       incr tabc;
      
       (match eo with
         (* infinite loop *)
         | None -> plsl (fun st -> tab(); gStmt st) ps;
         (* loop with conditional expression *)
         | Some e ->
           (tab();
            pstr "(if ";
            gExpr e true;
            pstr "\n";
            incr tabc;
            tab();
            pstr "(then $#continue\n";
            incr tabc;
            plsl (fun st -> tab(); gStmt st) ps;
            decr tabc;
            pstr ")\n";
            tab();
            pstr "(else (br $#break)))\n";
            decr tabc));
           
        tab();
        may (fun s -> gStmt s; pstr "\n"; tab()) po2;
        pstr "(br $#continue))\n";
        decr tabc;

    | SDecl_stmt(id_e_ls) ->
       let notUnder = List.filter (fun (x,_) -> x != "_") id_e_ls in
       let inCtx = List.filter (fun (x,_) -> in_context x ctx) notUnder in
       let newid_e_ls = List.map (fun (id,e) -> ("g#"^id,addHash e)) inCtx in
       List.iter (fun (id,(_,(_,t,_))) -> add id t ctx) newid_e_ls;

       plsl (fun (id, e) ->
           let (_,(_,typ,_)) = e in
           tab();
           fprintf oc "(set_local $%t %t)"
                   (fun c -> let depth = scope_depth (get_scope id ctx) in
                             pstr (alphaRenaming id depth typ))
                   (fun c -> gExpr e true))
            (* id_e_ls *)
            (List.map (fun (id,e) -> (id,addHash e)) id_e_ls)
            
    | Type_stmt(id_typ_ls) -> () 
    | Expr_stmt e -> gExpr e true       
    | Return(eo) -> 
        fprintf oc "(return %t)"
                (fun c-> defaulto (fun x -> gExpr x true) () eo)
    | Break -> pstr "(br $#break)";
    | Continue -> pstr "(br $#continue)";
    | Empty_stmt -> () (* pstr "nop" *) (* or should we not do anything? *)
  in
  let rec gDecl ((ud,pos): Typed.annotated_utdecl) = tab(); match ud with
           | Var_decl(xss) ->

              List.iter (plsl (fun (s,eo,typo) ->
               (match (typo,eo) with
                | (_,Some ((ue,(_,typ,_))  as e)) ->

                  pstr "(func $#global";
                  pstr (string_of_int !globc);
                  pstr "\n";
                  incr tabc;

                  let styp = gTyp typ in
                  let size = size_of_type typ in
                    tab();
                    fprintf oc "(%s.store (i32.const %d) %t)"
                                styp
                                !segc
                                (fun c -> gExpr e true);
                    Hashtbl.add globalVar s (styp, !segc);
                    segc := !segc + size;
                 
                  pstr ")";
                  decr tabc;
                  globc := !globc + 1
                 
                | (Some typ, None) ->
                  let styp = gTyp typ in
                  let size = size_of_type typ in
                    Hashtbl.add globalVar s (styp, !segc);
                    segc := !segc + size;
              
                | _ -> failwith "weeding error" ))) xss;

           | Type_decl(id_atyp_ls) -> ()
           | Func_decl(fId, id_typ_ls, typ, ps) -> 
              (* local variables must be declared at the function declaration *)
              (* write a function to go through the branch of the typed ast and gather all the variable declarations, then call it at the beginning *)
              if fId = "main" then mainFunc := true;
              pstr "(func $"; pstr fId;
              incr tabc; pstr "\n";
              psfl "\n"
                (fun (id,typ) ->
                  tab();
                  pstr (sprintf "(param $%s " (alphaRenaming id 2 typ));
                  pstr (gTyp typ);
                  pstr ")")
                id_typ_ls;
              (match typ with
                | TVoid -> ()
                | _     ->
                  tab();
                  pstr "(result ";
                  pstr (gTyp typ);
                  pstr ")\n");
              if Hashtbl.mem table fId then
               (let locals = Hashtbl.find table fId in
                plsl (fun (v,d,t,t2) ->
                       tab();
                       fprintf oc "(local $%t %t)"
                               (fun c -> (*pstr (sprintf "%s_%s_%d" v t d))*)
                                         pstr (alphaRenaming v d (sTyp t2)))
                               (fun c -> pstr (gTyp t2)))
                     locals; pstr "\n");
              plsl (fun st -> tab(); gStmt st) ps;
              decr tabc;
              pstr ")";

(* func:   ( func <name>? <type>? <param>* <result>? <local>* <expr>* ) *)
(* result: ( result <type> ) *)
  in
(* module:  ( module <type>* <func>* <import>* <export>* <table>* <memory>? <start>? ) *)
       fprintf oc
           ("(module\n"
           ^^"  (memory 1024 1024 %t)\n"

           ^^"  (import $#write_i32 \"spectest\" \"write\" (param i32))\n"
           ^^"  (import $#writeln_i32 \"spectest\" \"writeln\" (param i32))\n"

           ^^"  (import $#print_i32 \"spectest\" \"print\" (param i32))\n"
           ^^"  (import $#print_f64 \"spectest\" \"flprint\" (param f64))\n"
           ^^"  (import $#println_i32 \"spectest\" \"println\" (param i32))\n"
           ^^"  (import $#println_f64 \"spectest\" \"flprintln\" (param f64))\n"

           ^^"  (func $#writelni32 (param $i i32)\n"
           ^^"    (call_import $#writeln_i32\n"
           ^^"                 (get_local $i)))\n"

           ^^"  (func $#writei32 (param $i i32)\n"
           ^^"    (call_import $#write_i32\n"
           ^^"                 (get_local $i)))\n"

           ^^"  (func $#printlnbool (param $i i32)\n"
           ^^"    (call $#printbool (get_local $i))\n"
           ^^"    (call $#writei32 (i32.const 10)))\n"

           ^^"  (func $#printbool (param $i i32)\n"
           ^^"    (if (i32.ne (get_local $i) (i32.const 0))\n"
           ^^"        (then\n"
           ^^"          (call $#writei32 (i32.const 116))\n"
           ^^"          (call $#writei32 (i32.const 114))\n"
           ^^"          (call $#writei32 (i32.const 117))\n"
           ^^"          (call $#writei32 (i32.const 101))\n"
           ^^"        )\n"
           ^^"        (else\n"
           ^^"          (call $#writei32 (i32.const 102))\n"
           ^^"          (call $#writei32 (i32.const 97))\n"
           ^^"          (call $#writei32 (i32.const 108))\n"
           ^^"          (call $#writei32 (i32.const 115))\n"
           ^^"          (call $#writei32 (i32.const 101))\n"
           ^^"        )))\n"

           ^^"  (func $#printi32 (param $i i32)\n"
           ^^"    (call_import $#print_i32\n"
           ^^"                 (get_local $i)))\n"

           ^^"  (func $#printf64 (param $i f64)\n"
           ^^"    (call_import $#print_f64\n"
           ^^"                 (get_local $i)))\n"

           ^^"  (func $#printlni32 (param $i i32)\n"
           ^^"    (call_import $#println_i32\n"
           ^^"                 (get_local $i)))\n"

           ^^"  (func $#printlnf64 (param $i f64)\n"
           ^^"    (call_import $#println_f64\n"
           ^^"                 (get_local $i)))\n"

           ^^"  (func $#printlnstring (param $s i32)\n"
           ^^"                        (local $n i32)\n"
           ^^"                        (local $i i32)\n"
           ^^"    (set_local $i (i32.const 0))\n"
           ^^"    (set_local $n (i32.load (get_local $s)))\n"
           ^^"    (loop $#break $#continue\n"
           ^^"      (br_if $#break (i32.ge_u (get_local $i) (get_local $n)))\n" 
           ^^"      (call $#writei32 (i32.load (i32.add (get_local $s) (i32.mul (i32.const 4) (i32.add (i32.const 1) (get_local $i))))))\n"
           ^^"      (set_local $i (i32.add (get_local $i) (i32.const 1)))\n"
           ^^"      (br $#continue))\n"
           ^^"    (call $#writei32 (i32.const 10)))\n\n"

           ^^"  (func $#printstring (param $s i32)\n"
           ^^"                      (local $n i32)\n"
           ^^"                      (local $i i32)\n"
           ^^"    (set_local $i (i32.const 0))\n"
           ^^"    (set_local $n (i32.load (get_local $s)))\n"
           ^^"    (loop $#break $#continue\n"
           ^^"      (br_if $#break (i32.ge_u (get_local $i) (get_local $n)))\n" 
           ^^"      (call $#writei32 (i32.load (i32.add (get_local $s) (i32.mul (i32.const 4) (i32.add (i32.const 1) (get_local $i))))))\n"
           ^^"      (set_local $i (i32.add (get_local $i) (i32.const 1)))\n"
           ^^"      (br $#continue)))\n\n"

           ^^"  (func $#string_eq (param $s1 i32)\n"
           ^^"                    (param $s2 i32)\n"
           ^^"                    (local $s1len i32)\n"
           ^^"                    (local $s2len i32)\n"
           ^^"                    (local $r i32)\n"
           ^^"                    (local $tmp i32)\n"
           ^^"                    (local $i i32)\n"
           ^^"                    (result i32)\n"
           ^^"    (set_local $i (i32.const 1))\n"
           ^^"    (set_local $s1len (i32.load (get_local $s1)))\n"
           ^^"    (set_local $s2len (i32.load (get_local $s2)))\n"
           ^^"    (if (i32.ne (get_local $s1len) (get_local $s2len))\n"
           ^^"        (return (i32.const 0)))\n"
           ^^"    (loop $#break $#continue\n"
           ^^"     (if (i32.gt_s (get_local $i) (get_local $s1len))\n"
           ^^"         (return (i32.const 1)))\n"
           ^^"     (set_local $tmp (i32.mul (i32.const 4) (get_local $i)))\n"
           ^^"     (set_local $r (i32.ne (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                              (get_local $s1)))\n"
           ^^"                           (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                              (get_local $s2)))))\n"
           ^^"     (if (get_local $r) (return (i32.const 0)))\n"
           ^^"     (set_local $i (i32.add (get_local $i) (i32.const 1)))\n"
           ^^"     (br $#continue)))\n\n"

           ^^"  (func $#string_ne (param $s1 i32)\n"
           ^^"                    (param $s2 i32)\n"
           ^^"                    (local $s1len i32)\n"
           ^^"                    (local $s2len i32)\n"
           ^^"                    (local $r i32)\n"
           ^^"                    (local $tmp i32)\n"
           ^^"                    (local $i i32)\n"
           ^^"                    (result i32)\n"
           ^^"    (set_local $i (i32.const 1))\n"
           ^^"    (set_local $s1len (i32.load (get_local $s1)))\n"
           ^^"    (set_local $s2len (i32.load (get_local $s2)))\n"
           ^^"    (if (i32.ne (get_local $s1len) (get_local $s2len))\n"
           ^^"        (return (i32.const 1)))\n"
           ^^"    (loop $#break $#continue\n"
           ^^"     (if (i32.gt_s (get_local $i) (get_local $s1len))\n"
           ^^"         (return (i32.const 0)))\n"
           ^^"     (set_local $tmp (i32.mul (i32.const 4) (get_local $i)))\n"
           ^^"     (set_local $r (i32.ne (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                              (get_local $s1)))\n"
           ^^"                           (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                              (get_local $s2)))))\n"
           ^^"     (if (get_local $r) (return (i32.const 1)))\n"
           ^^"     (set_local $i (i32.add (get_local $i) (i32.const 1)))\n"
           ^^"     (br $#continue)))\n\n"

           ^^"  (func $#string_lt (param $s2 i32)\n"
           ^^"                    (param $s1 i32)\n"
           ^^"                    (local $s1len i32)\n"
           ^^"                    (local $s2len i32)\n"
           ^^"                    (local $r i32)\n"
           ^^"                    (local $tmp i32)\n"
           ^^"                    (local $i i32)\n"
           ^^"                    (result i32)\n"
           ^^"    (set_local $i (i32.const 1))\n"
           ^^"    (set_local $s1len (i32.load (get_local $s1)))\n"
           ^^"    (set_local $s2len (i32.load (get_local $s2)))\n"
           ^^"    (loop $#break $#continue\n"
           ^^"     (if (i32.gt_s (get_local $i) (get_local $s2len))\n"
           ^^"         (return (i32.gt_s (get_local $s1len) (get_local $s2len))))\n"
           ^^"     (if (i32.gt_s (get_local $i) (get_local $s1len))\n"
           ^^"         (return (i32.const 0)))\n"
           ^^"     (set_local $tmp (i32.mul (i32.const 4) (get_local $i)))\n"
           ^^"     (set_local $r (i32.ne (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                              (get_local $s1)))\n"
           ^^"                           (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                              (get_local $s2)))))\n"
           ^^"     (if (get_local $r)\n"
           ^^"         (return (i32.gt_s (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                              (get_local $s1)))\n"
           ^^"                           (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                              (get_local $s2))))))\n"
           ^^"     (set_local $i (i32.add (get_local $i) (i32.const 1)))\n"
           ^^"     (br $#continue)))\n\n"

           ^^"  (func $#string_gt (param $s1 i32)\n"
           ^^"                    (param $s2 i32)\n"
           ^^"                    (local $s1len i32)\n"
           ^^"                    (local $s2len i32)\n"
           ^^"                    (local $r i32)\n"
           ^^"                    (local $tmp i32)\n"
           ^^"                    (local $i i32)\n"
           ^^"                    (result i32)\n"
           ^^"    (set_local $i (i32.const 1))\n"
           ^^"    (set_local $s1len (i32.load (get_local $s1)))\n"
           ^^"    (set_local $s2len (i32.load (get_local $s2)))\n"
           ^^"    (loop $#break $#continue\n"
           ^^"     (if (i32.gt_s (get_local $i) (get_local $s2len))\n"
           ^^"         (return (i32.gt_s (get_local $s1len) (get_local $s2len))))\n"
           ^^"     (if (i32.gt_s (get_local $i) (get_local $s1len))\n"
           ^^"         (return (i32.const 0)))\n"
           ^^"     (set_local $tmp (i32.mul (i32.const 4) (get_local $i)))\n"
           ^^"     (set_local $r (i32.ne (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                              (get_local $s1)))\n"
           ^^"                           (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                              (get_local $s2)))))\n"
           ^^"     (if (get_local $r)\n"
           ^^"         (return (i32.gt_s (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                              (get_local $s1)))\n"
           ^^"                           (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                              (get_local $s2))))))\n"
           ^^"     (set_local $i (i32.add (get_local $i) (i32.const 1)))\n"
           ^^"     (br $#continue)))\n\n"

           ^^"  (func $#string_ge (param $s2 i32)\n"
           ^^"                    (param $s1 i32)\n"
           ^^"                    (local $s1len i32)\n"
           ^^"                    (local $s2len i32)\n"
           ^^"                    (local $r i32)\n"
           ^^"                    (local $tmp i32)\n"
           ^^"                    (local $i i32)\n"
           ^^"                    (result i32)\n"
           ^^"    (set_local $i (i32.const 1))\n"
           ^^"    (set_local $s1len (i32.load (get_local $s1)))\n"
           ^^"    (set_local $s2len (i32.load (get_local $s2)))\n"
           ^^"    (loop $#break $#continue\n"
           ^^"     (if (i32.gt_s (get_local $i) (get_local $s1len))\n"
           ^^"         (return (i32.le_s (get_local $s1len) (get_local $s2len))))\n"
           ^^"     (if (i32.gt_s (get_local $i) (get_local $s2len))\n"
           ^^"         (return (i32.const 0)))\n"
           ^^"     (set_local $tmp (i32.mul (i32.const 4) (get_local $i)))\n"
           ^^"     (set_local $r (i32.gt_s (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                                (get_local $s1)))\n"
           ^^"                             (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                                (get_local $s2)))))\n"
           ^^"     (if (get_local $r) (return (i32.const 0)))\n"
           ^^"     (set_local $i (i32.add (get_local $i) (i32.const 1)))\n"
           ^^"     (br $#continue)))\n\n"

           ^^"  (func $#string_le (param $s1 i32)\n"
           ^^"                    (param $s2 i32)\n"
           ^^"                    (local $s1len i32)\n"
           ^^"                    (local $s2len i32)\n"
           ^^"                    (local $r i32)\n"
           ^^"                    (local $tmp i32)\n"
           ^^"                    (local $i i32)\n"
           ^^"                    (result i32)\n"
           ^^"    (set_local $i (i32.const 1))\n"
           ^^"    (set_local $s1len (i32.load (get_local $s1)))\n"
           ^^"    (set_local $s2len (i32.load (get_local $s2)))\n"
           ^^"    (loop $#break $#continue\n"
           ^^"     (if (i32.gt_s (get_local $i) (get_local $s1len))\n"
           ^^"         (return (i32.le_s (get_local $s1len) (get_local $s2len))))\n"
           ^^"     (if (i32.gt_s (get_local $i) (get_local $s2len))\n"
           ^^"         (return (i32.const 0)))\n"
           ^^"     (set_local $tmp (i32.mul (i32.const 4) (get_local $i)))\n"
           ^^"     (set_local $r (i32.gt_s (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                                (get_local $s1)))\n"
           ^^"                             (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                                (get_local $s2)))))\n"
           ^^"     (if (get_local $r) (return (i32.const 0)))\n"
           ^^"     (set_local $i (i32.add (get_local $i) (i32.const 1)))\n"
           ^^"     (br $#continue)))\n\n"

           ^^"  (func $#string_cat (param $s1 i32)\n"
           ^^"                     (param $s2 i32)\n"
           ^^"                     (local $s1len i32)\n"
           ^^"                     (local $s2len i32)\n"
           ^^"                     (local $base i32)\n"
           ^^"                     (local $ptr i32)\n"
           ^^"                     (local $i i32)\n"
           ^^"                     (result i32)\n"
           ^^"    (set_local $base (i32.load (i32.const 0)))\n"
           ^^"    (set_local $ptr (i32.add (get_local $base) (i32.const 4)))\n"
           ^^"    (set_local $s1len (i32.load (get_local $s1)))\n"
           ^^"    (set_local $s2len (i32.load (get_local $s2)))\n"
           
           ^^"    (i32.store (get_local $base) (i32.add (get_local $s1len) (get_local $s2len)))\n"

           ^^"    (set_local $i (i32.const 0))\n"
           ^^"    (set_local $s1 (i32.add (i32.const 4) (get_local $s1)))\n"
           ^^"    (loop $#break $#continue\n"
           ^^"     (if (i32.ge_s (get_local $i) (get_local $s1len))\n"
           ^^"         (br $#break))\n"
           ^^"     (i32.store (get_local $ptr)\n"
           ^^"                (i32.load (get_local $s1)))\n"
           ^^"     (set_local $i   (i32.add (i32.const 1) (get_local $i)))\n"
           ^^"     (set_local $s1  (i32.add (i32.const 4) (get_local $s1)))\n"
           ^^"     (set_local $ptr (i32.add (i32.const 4) (get_local $ptr)))\n"
           ^^"     (br $#continue))\n\n"

           ^^"    (set_local $i (i32.const 0))\n"
           ^^"    (set_local $s2 (i32.add (i32.const 4) (get_local $s2)))\n"
           ^^"    (loop $#break $#continue\n"
           ^^"     (if (i32.ge_s (get_local $i) (get_local $s2len))\n"
           ^^"         (br $#break))\n"
           ^^"     (i32.store (get_local $ptr)\n"
           ^^"                (i32.load (get_local $s2)))\n"
           ^^"     (set_local $i   (i32.add (i32.const 1) (get_local $i)))\n"
           ^^"     (set_local $s2  (i32.add (i32.const 4) (get_local $s2)))\n"
           ^^"     (set_local $ptr (i32.add (i32.const 4) (get_local $ptr)))\n"
           ^^"     (br $#continue))\n\n"
           ^^"    (i32.store (i32.const 0) (get_local $ptr))\n"
           ^^"    (return (get_local $base)))\n"


           ^^"  (func $#nequals_bytes (param $s1 i32) (param $s2 i32) (param $len i32) (local $i i32) (local $tmp i32) (result i32)\n"
           ^^"    (set_local $i (i32.const 0))\n"
           ^^"    (loop $#break $#continue\n"
           ^^"     (if (i32.ge_s (get_local $i) (get_local $len))\n"
           ^^"         (return (i32.const 0)))\n"
           ^^"     (set_local $tmp (i32.mul (i32.const 4) (get_local $i)))\n"
           ^^"     (set_local $tmp (i32.ne (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                                (get_local $s1)))\n"
           ^^"                             (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                                (get_local $s2)))))\n"
           ^^"     (if (get_local $tmp) (return (i32.const 1)))\n"
           ^^"     (set_local $i (i32.add (get_local $i) (i32.const 1)))\n"
           ^^"     (br $#continue)))\n\n"

           ^^"  (func $#equals_bytes (param $s1 i32) (param $s2 i32) (param $len i32) (local $i i32) (local $tmp i32) (result i32)\n"
           ^^"    (set_local $i (i32.const 0))\n"
           ^^"    (loop $#break $#continue\n"
           ^^"     (if (i32.ge_s (get_local $i) (get_local $len))\n"
           ^^"         (return (i32.const 1)))\n"
           ^^"     (set_local $tmp (i32.mul (i32.const 4) (get_local $i)))\n"
           ^^"     (set_local $tmp (i32.ne (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                                (get_local $s1)))\n"
           ^^"                             (i32.load (i32.add (get_local $tmp)\n"
           ^^"                                                (get_local $s2)))))\n"
           ^^"     (if (get_local $tmp) (return (i32.const 0)))\n"
           ^^"     (set_local $i (i32.add (get_local $i) (i32.const 1)))\n"
           ^^"     (br $#continue)))\n\n"

           ^^"  (func $#copyarg (param $src i32)\n"
           ^^"               (param $len i32)\n"
           ^^"               (local $base i32)\n"
           ^^"               (result i32)\n"
           ^^"    (set_local $base (i32.load (i32.const 0)))\n"
           ^^"    (call $#copy (get_local $src) (get_local $base) (get_local $len))\n"
           ^^"    (i32.store (i32.const 0) (i32.add (get_local $base) (get_local $len)))\n"
           ^^"    (return (get_local $base)))\n"

           ^^"  (func $#copy (param $src i32)\n"
           ^^"               (param $dest i32)\n"
           ^^"               (param $len i32)\n"
           ^^"               (local $i i32)\n"
           ^^"               (local $n i32)\n"
           ^^"               (result i32)\n"
           ^^"    (set_local $i (i32.const 0))\n"
           ^^"    (loop $#break $#continue\n"
           ^^"      (br_if $#break (i32.ge_u (get_local $i) (get_local $len)))\n" 
           ^^"      (i32.store (get_local $dest)\n"
           ^^"                 (i32.load (get_local $src)))\n"
           ^^"      (set_local $i (i32.add (i32.const 4) (get_local $i)))\n"
           ^^"      (set_local $src (i32.add (i32.const 4) (get_local $src)))\n"
           ^^"      (set_local $dest (i32.add (i32.const 4) (get_local $dest)))\n"
           ^^"      (br $#continue))\n\n"
           ^^"    (return (i32.sub (get_local $dest)\n"
           ^^"                     (get_local $i))))\n"
           ^^" %t\n"
           ^^"  (start $#init)\n"
           ^^"  (func $#init\n"
           ^^"    (i32.store (i32.const 0) (i32.const %t))\n" (* Heap pointer *)
           ^^"%t%t))")

       (fun c -> pstr "")

       (fun c -> incr tabc;
                 plsl gDecl decls;
                 decr tabc)
               
       (fun c -> pstr (string_of_int !segc))
       
       (fun c -> incr tabc; incr tabc;
                 if !globc > 0 then begin
                   for i = 0 to !globc - 1 do
                     tab(); fprintf oc "(call $#global%d)\n" i;
                   done;
                 end)
 
       (fun c -> if !mainFunc then begin
                   tab();
                   fprintf oc "(call $main)";
                 end)
