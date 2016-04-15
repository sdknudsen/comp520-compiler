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
  (* let pcsl f = function (* print comma separated list *)
    | [] -> ()
    | x::xs -> f x; List.iter (fun y -> pstr ", "; f y) xs
  in *)
  let plsl f = function (* print comma separated list *)
    | [] -> ()
    | x::xs -> f x; List.iter (fun y -> pstr "\n"; f y) xs
  in
  let gBOp op =
    let s = match op with
    | Equals -> "eq"
    | Notequals -> "ne"
    | Lt -> "lt_s"
    | Lteq -> "le_s"
    | Gt -> "gt_s"
    | Gteq -> "ge_s"
    | Plus -> "add"
    | Minus -> "sub"
    | Bitor -> "or"
    | Bitand -> "and"
    | Bitxor -> "xor"
    | Times -> "mul"
    | Div -> "div_s"
    | Modulo -> "rem_s"
    | Lshift -> "shl_s"
    | Rshift -> "shr_s"
    | Boolor -> raise (GenError "boolor implemented")
    | Booland -> raise (GenError "booland implemented")
    | Bitnand -> raise (GenError "bitnand implemented")
    in pstr s
  in
  
  (* let rec name_typ (at:Typed.uttyp) = match at with
    | TSimp("bool", _)    -> "bool"
    | TSimp("int", _)     -> "int"
    | TSimp("float64", _) -> "float64"
    | TSimp("rune", _)    -> "rune"
    | TSimp("string", _)  -> "string"

    | TSimp(_, _)    -> failwith "Named types not yet supported"
    | TStruct(_)
    | TArray(_,_)
    | TSlice(_)
    | TFn(_,_) -> failwith "Structured types not yet supported"
    | TVoid -> ""
    | TKind(a) -> ""
  in *)
  let rec gTyp (at:Typed.uttyp) = match at with
    (* do we need to check if the base names have been aliased?? *)
    | TSimp("bool", _)    -> pstr "i32"
    | TSimp("int", _)     -> pstr "i32"
    | TSimp("float64", _) -> pstr "f64"
    | TSimp("rune", _)    -> pstr "i32"
    | TSimp("string", _)    -> failwith "not implemented"
    (* I'm not sure about this: *)
    | TSimp(name,ctx) -> (match find name ctx with
                              | None -> raise (GenError "Base type not found")
                              | Some e -> gTyp e)
    | TStruct(_)
    | TArray(_,_)
    | TSlice(_)
    | TFn(_,_) -> failwith "Structured types not yet supported"
    | TVoid -> () (* is this right?? *)
    | TKind(a) -> gTyp a
  in

  let rec value_bytes v = match v with
    | SValue(r,id) -> failwith "avalues not yet supported"
    | ILit(d) -> 4
    | FLit(f) -> 4
    | RLit(c) -> 4

    (* Strings are encoded as a size `n` followed by `n` runes *)
    | AValue(r,e) -> 4
    | SLit(s) -> 4 + (String.length s) * 4
    | _ -> 0
  in
  let rec alphaRenaming id d (at:Typed.uttyp) : string = match at with
    (* get wast type before printing !! *)
    | TSimp(t,_) -> sprintf "%s_%s_%d" id t d
    | TArray(_,_)
    | TStruct(_)
    | TFn(_,_)
    | TSlice(_)
    | TVoid
    | TKind(_) -> failwith "kinds not yet supported"
(* type:   ( type <var> ) *)
(* type:    ( type <name>? ( func <param>* <result>? ) ) *)
  in
  let rec gExpr ((ue,(pos,typ,ctx)):Typed.annotated_texpr) =
    match ue with
    | Iden(id) ->
      let depth = scope_depth (get_scope id ctx) in
        if depth > 0 then
          fprintf oc "(get_local $%t)"
                     (fun c -> pstr (alphaRenaming id depth typ))
        else begin
          let global = Hashtbl.find globalVar id in
             fprintf oc "(%s.load (i32.const %d))" (fst global) (snd global)
        end
    | AValue(r,e) -> failwith "avalues not yet supported"
    | SValue(r,id) -> failwith "avalues not yet supported"
    (* | Parens(e)  -> fprintf oc "(%t)" (fun c -> gExpr e) *)
    | ILit(d) -> fprintf oc "(i32.const %d)" d
    | FLit(f) -> fprintf oc "(f64.const %f)" f
    | RLit(c) -> fprintf oc "(i32.const %d)" (int_of_char c)
    | SLit(s) -> fprintf oc "(block (i32.store (i32.load (i32.const 0)) (i32.const %d)) %t (i32.store (i32.const 0) (i32.add (i32.const %d) (i32.load (i32.const 0)))) (i32.sub (i32.load (i32.const 0)) (i32.const %d)))"
                  (String.length s)
                  (fun c -> String.iteri
                             (fun i c -> (fprintf oc "\n(i32.store (i32.add (i32.load (i32.const 0)) (i32.const %d)) (i32.const %d))" ((i+1) * 4) (Char.code c)))
                             s)
                  (((String.length s) + 1) * 4)
                  (((String.length s) + 1) * 4)
       (* gExpr (Bexp(Bitand,1,(Bitor,e1,e2)),(pos,typ,ctx)) *)
       (* gExpr (Bexp(Bitand,1,(Bitand,e1,e2)),(pos,typ,ctx)) *)
       (* gExpr (Uexp(Bitnot,(Bitand,e1,e2)),(pos,typ,ctx)) *)

    | Bexp(Boolor,e1,e2) ->
       fprintf oc "(i32.and (i32.const 1) (i32.or %t %t))"
               (* do something with the type? *)
               (* (fun c -> gTyp typ) *)
               (fun c -> gExpr e1)
               (fun c -> gExpr e2)
    | Bexp(Booland,e1,e2) -> 
       fprintf oc "(i32.and (i32.const 1) (i32.and %t %t))"
               (fun c -> gExpr e1)
               (fun c -> gExpr e2)
    | Bexp(Bitnand,e1,e2) -> 
       fprintf oc "(i32.neg (i32.const 1) (i32.and %t %t))"
               (fun c -> gExpr e1)
               (fun c -> gExpr e2)

    | Bexp(op,e1,e2) ->
       fprintf oc "(%t.%t %t %t)"
                      (fun c -> gTyp typ)
                      (fun c -> gBOp op)
                      (fun c -> gExpr e1)
                      (fun c -> gExpr e2)
       
    (* | Uexp(Negative, ((_,(_,TSimp("int",_),_)) as e) ) ->  *)
    (* check the type? *)
    | Uexp(Negative, e) -> 
       fprintf oc "(i32.sub (i32.const 0) %t)"
                  (fun c -> gExpr e)
    (* | Uexp(Boolnot, ((_,(_,TSimp("int",_),_)) as e) ) ->  *)
    | Uexp(Boolnot, e) -> 
       fprintf oc "(i32.and (i32.const 1) (i32.xor (i32.const 4294967295) %t))" (* 2^32 - 1 *)
                  (fun c -> gExpr e)
    (* | Uexp(Bitnot, ((_,(_,TSimp("int",_),_)) as e) ) ->  *)
    | Uexp(Bitnot, e) -> 
       fprintf oc "(i32.xor (i32.const 4294967295) %t)"
                  (fun c -> gExpr e)
    | Uexp(Positive,e) -> gExpr e
       (* fprintf oc "(%t.%t %t)" *)
       (*                (fun c -> gTyp typ) *)
       (*                (fun c -> gUOp op) *)
    | Fn_call((Iden(i),_), k) -> fprintf oc "(call $%t %t)"
                                            (fun c -> pstr i)
                                            (fun c -> pssl " " gExpr k)
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
  let rec gStmt ((us, (pos,ctx)): Typed.annotated_utstmt) =
    match us with
    | Assign(xs, es) -> 
       plsl (fun (v,e) -> let (ue,(_,typ,_)) = v in
                          (match ue with
                          | Iden(id) -> let depth = scope_depth (get_scope id ctx) in
                                        if depth > 0 then
                                          fprintf oc "(set_local $%t %t)"
                                            (fun c -> pstr (getId v))
                                            (fun c -> gExpr e)
                                        else begin
                                          let styp = ref "" in
                                          let slen = ref 100 in
                                          (match typ with
                                          | TSimp("float64", _) -> styp := "f64"
                                          | TSimp("int", _)
                                          | TSimp("rune", _)
                                          | TSimp("bool", _) -> styp := "i32"
                                          | _ -> failwith "not implemented");
                                          fprintf oc "(%s.store (i32.const %d) %t)"
                                            !styp !segc (fun c -> gExpr e);
                                          (match fst e with
                                          | Iden(id) ->
                                              (match id with
                                              | "true" -> slen := 1
                                              | "false" -> slen := 1
                                              | _ -> ())
                                          | ILit(d) -> slen := String.length (string_of_int d)
                                          | FLit(f) -> slen := String.length (string_of_float f)
                                          | RLit(c) -> slen := String.length (string_of_int (int_of_char c))
                                          | _ -> ());
                                          Hashtbl.replace globalVar id (!styp, !segc);
                                          segc := !segc + !slen
                                        end
                          | _ -> failwith "Found non id in lhs of assignment"))
                            
         (zip xs es)
    | Var_stmt(xss) ->
       List.iter (plsl (fun (s,eo,typo) ->
        match (typo,eo) with
        | (Some typ,Some e) -> let depth = scope_depth (get_scope s ctx) in
                               fprintf oc "(set_local $%t %t)"
                                 (fun c -> pstr (alphaRenaming s depth typ))
                                 (fun c -> gExpr e)
        | (None,Some e) -> let (_,(_,typ,_)) = e in
                           let depth = scope_depth (get_scope s ctx) in
                           fprintf oc "(set_local $%t %t)"
                             (fun c -> pstr (alphaRenaming s depth typ))
                             (fun c -> gExpr e)
        | (Some typ,None) -> failwith "this shouldn't happen?"(*this or unit??*)
        | _ -> failwith "weeding error"
       )) xss
       (* let ls = List.map (fun () -> ) (List.concat xss) in *)
       (* plsl (fun (v,e) -> fprintf oc "(set_local $%t %t)" *)
       (*                            (fun c -> pstr (getId v)) *)
       (*                            (fun c -> gExpr e)) ls *)

    | Print(es) ->
       List.iter
         (function
           | (_, (_,TSimp("bool",_),_)) as e -> 
               pstr "(call $#printbool ";
               gExpr e;
               pstr ")"
           | (_, (_,TSimp("int",_),_)) as e -> 
               pstr "(call $#printi32 ";
               gExpr e;
               pstr ")"
           | (_, (_,TSimp("float64",_),_)) as e -> 
               pstr "(call $#printf64";
               gExpr e;
               pstr ")"
           | (_, (_,TSimp("rune",_),_)) as e -> 
               pstr "(call $#writei32 ";
               gExpr e;
               pstr ")"
           | (_, (_,TSimp("string",_),_)) as e -> 
               pstr "(call $#printstring ";
               gExpr e;
               pstr ")"
           | _ -> failwith "Print of unimplemented type") 
         es
       
    | Println(es) ->
       List.iter
         (function
           | (_, (_,TSimp("bool",_),_)) as e -> 
               pstr "(call $#printlnbool ";
               gExpr e;
               pstr ")"
           | (_, (_,TSimp("int",_),_)) as e -> 
               pstr "(call $#printlni32 ";
               gExpr e;
               pstr ")"
           | (_, (_,TSimp("float64",_),_)) as e -> 
               pstr "(call $#printlnf64";
               gExpr e;
               pstr ")"
           | (_, (_,TSimp("rune",_),_)) as e -> 
               pstr "(call $#writelni32 ";
               gExpr e;
               pstr ")"
           | (_, (_,TSimp("string",_),_)) as e -> 
               pstr "(call $#printlnstring ";
               gExpr e;
               pstr ")"
           | _ -> failwith "Println of unimplemented type") 
         es
        
    | If_stmt(po,e,ps,pso) ->
       may (fun s -> gStmt s; pstr "\n"; tab()) po;
       pstr "(if ";
       gExpr e;
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
                        | None -> gExpr e; pstr "\n"
                        | Some t -> pstr "(i32.eq ";
                                    gExpr t;
                                    pstr " ";
                                    gExpr e;
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
            gExpr e;
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

  (* ( loop <label1>? <label2>? <expr>* ) *)
    | SDecl_stmt(id_e_ls) ->
        plsl (fun (id, e) ->
              let (_,(_,typ,_)) = e in
              tab();
              fprintf oc "(set_local $%t %t)"
                (fun c -> let depth = scope_depth (get_scope id ctx) in
                          pstr (alphaRenaming id depth typ))
                (fun c -> gExpr e))
          id_e_ls
        
    | Type_stmt(id_typ_ls) -> () 
    | Expr_stmt e -> gExpr e        
    | Return(eo) -> 
        fprintf oc "(return %t)"
                (fun c-> defaulto gExpr () eo)
  (* ( return <expr>? ) *)
    | Break -> pstr "(br $#break)";
    | Continue -> pstr "(br $#continue)";
    | Empty_stmt -> () (* pstr "nop" *) (* or should we not do anything? *)
  in
  let rec gDecl ((ud,pos): Typed.annotated_utdecl) = tab(); match ud with
           | Var_decl(xss) ->
              pstr "(func $#global";
              pstr (string_of_int !globc);
              pstr "\n";
              incr tabc;

              List.iter (plsl (fun (s,eo,typo) ->
               (let styp = ref "" in
                let size = ref 0 in
                (match (typo,eo) with
                | (_,Some e) ->
                  (let (ue,(_,typ,_)) = e in begin
                    (match typ with
                      | TSimp("float64", _) ->
                        styp := "f64";
                        size := 8;
                        fprintf oc "(f64.store (i32.const %d) %t)"
                                !segc
                                (fun c -> gExpr e);
                       | TSimp("int", _)
                      | TSimp("rune", _)
                      | TSimp("bool", _)
                      | TSimp("string", _) ->
                        styp := "i32";
                        size := 4;
                        fprintf oc "(i32.store (i32.const %d) %t)"
                                !segc
                                (fun c -> gExpr e);
                      | _ -> failwith "not implemented");
                    Hashtbl.add globalVar s (!styp, !segc);
                    segc := !segc + !size;
                  end);

                | (Some typ, None) ->
                    (match typ with
                      | TSimp("float64", _) -> styp := "f64"; size := 8;
                      | TSimp("int", _)
                      | TSimp("rune", _)
                      | TSimp("string", _)
                      | TSimp("bool", _) ->    styp := "i32"; size := 4;
                      | _ -> failwith "not implemented");
                    Hashtbl.add globalVar s (!styp, !segc);
                    segc := !segc + !size;
              
                | _ -> failwith "weeding error" )))) xss;

              tab();
              pstr ")";
              decr tabc;
              globc := !globc + 1
           | Type_decl(id_atyp_ls) -> ()
           | Func_decl(fId, id_typ_ls, typ, ps) -> 
              (* local variables must be declared at the function declaration *)
              (* write a function to go through the branch of the typed ast and gather all the variable declarations, then call it at the beginning *)
              pstr "(func $"; pstr fId;
              incr tabc; pstr "\n";
              psfl "\n"
                (fun (id,typ) ->
                  tab();
                  pstr (sprintf "(param $%s " (alphaRenaming id 1 typ));
                  gTyp typ;
                  pstr ")")
                id_typ_ls;
              (match typ with
                | TVoid -> ()
                | _     ->
                  tab();
                  pstr "(result ";
                  gTyp typ;
                  pstr ")\n");
              if Hashtbl.mem table fId then
               (let locals = Hashtbl.find table fId in
                plsl (fun (v,d,t,t2) ->
                       tab();
                       fprintf oc "(local $%t %t)"
                               (fun c -> pstr (sprintf "%s_%s_%d" v t d))
                               (fun c -> gTyp t2))
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
           ^^"  (memory 128 128 %t)\n"

           ^^"  (import $#write_i32 \"spectest\" \"write\" (param i32))\n"
           ^^"  (import $#writeln_i32 \"spectest\" \"writeln\" (param i32))\n"

           ^^"  (import $#print_i32 \"spectest\" \"print\" (param i32))\n"
           ^^"  (import $#print_f64 \"spectest\" \"print\" (param f64))\n"
           ^^"  (import $#println_i32 \"spectest\" \"println\" (param i32))\n"
           ^^"  (import $#println_f64 \"spectest\" \"println\" (param f64))\n"

           ^^"  (func $#writelni32 (param $i i32)\n"
           ^^"    (call_import $#writeln_i32\n"
           ^^"                 (get_local $i)))\n"

           ^^"  (func $#writei32 (param $i i32)\n"
           ^^"    (call_import $#write_i32\n"
           ^^"                 (get_local $i)))\n"

           ^^"  (func $#prinlntbool (param $i i32)\n"
           ^^"    (call $#printbool (get_local $i))\n"
           ^^"    (call $#writei32 (i32.const 10)))\n"

           ^^"  (func $#printbool (param $i i32)\n"
           ^^"    (if (i32.eq (get_local $i) (i32.const 0))\n"
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

           ^^"  (start $#init)\n"
           ^^"  (func $#init\n"
           ^^"    %t\n"
           ^^"    (i32.store (i32.const 0) (i32.const %d))\n" (* Heap pointer *)
           ^^"    (call $main))\n\n"

           ^^"%t)")
       (fun c -> pstr "")

       (fun c -> if !globc > 0 then begin
                   for i = 0 to !globc - 1 do
                     tab(); fprintf oc "(call $#global%d)" i; pstr "\n";  
                   done;
                 end)
 
       !segc

       (fun c -> incr tabc;
                 plsl gDecl decls;
                 decr tabc)

(* more about webassembly: *)

(* value: <int> | <float> *)
(* var: <int> | $<name> *)
(* name: (<letter> | <digit> | _ | . | + | - | * | / | \ | ^ | ~ | = | < | > | ! | ? | @ | # | $ | % | & | | | : | ' | `)+ *)
(* string: "(<char> | \n | \t | \\ | \' | \" | \<hex><hex>)*" *)

(* type: i32 | i64 | f32 | f64 *)

(* unop:  ctz | clz | popcnt | ... *)
(* binop: add | sub | mul | ... *)
(* relop: eq | ne | lt | ... *)
(* sign: s|u *)
(* offset: offset=<uint> *)
(* align: align=(1|2|4|8|...) *)
(* cvtop: trunc_s | trunc_u | extend_s | extend_u | ... *)
