open Ast

(* let ppTable gamma outc =
  let str_of_typ = function
    | TInt -> "int"
    | TFloat -> "float"
    | TString -> "string" in
  Ctx.iter (fun k v -> Printf.fprintf outc "%s" (k ^ " : " ^ str_of_typ v ^ "\n")) gamma *)
let uop_to_str = function
  | BANG -> "!"           
  | MINUS -> "-"           

let bop_to_str = function
  | PLUS -> "+"           
  | MINUS -> "-"           
  | TIMES -> "*"           
  | DIV -> "/"           
  | PERCENT -> "%"           
  | BITAND -> "&"           
  | BITOR -> "|"           
  | CIRCUMFLEX -> "^"           
  | LCHEVRON -> "<"           
  | RCHEVRON -> ">"           
  | ASSIGNMENT -> "="           
  | LPAREN -> "("           
  | RPAREN -> ")"           
  | LBRACKET -> "["           
  | RBRACKET -> "]"           
  | LBRACE -> "{"           
  | RBRACE -> "}"           
  | COMMA -> ","           
  | DOT -> "."           
  | SEMICOLON -> ";"           
  | COLON -> ":"           
  | LSHIFT -> "<<"          
  | RSHIFT -> ">>"          
  | BITNAND -> "&^"          
  | PLUSEQ -> "+="          
  | MINUSEQ -> "-="          
  | TIMESEQ -> "*="          
  | DIVEQ -> "/="          
  | PERCENTEQ -> "%="          
  | AMPEQ -> "&="          
  | BITOREQ -> "|="          
  | BITNOTEQ -> "^="          
  | LSHIFTEQ -> "<<="         
  | RSHIFTEQ -> ">>="         
  | BITNANDEQ -> "&^="         
  | BOOL_AND -> "&&"          
  | BOOL_OR -> "||"          
  | LARROW -> "<-"          
  | INC -> "++"          
  | DEC -> "--"          
  | EQUALS -> "=="          
  | NOTEQUALS -> "!="          
  | LTEQ -> "<="          
  | GTEQ -> ">="          
  | COLONEQ -> ":="          
  | ELLIPSIS -> "..."         

let may f = function
  | Some typ -> f typ
  | None -> ()
              
let ppTree (TProg(pkg,stmts)) outc =
  let tabCount = ref 0 in
  let println() = Printf.fprintf outc "\n" in
  let ppStr s = Printf.fprintf outc "%s" s in
  let rec tabWith n = if n <= 0 then () else (ppStr "\t"; tabWith (n-1)) in
  let tab() = tabWith !tabCount in
  let pcsl = function
    | [] -> ()
    | x::xs -> ppStr x; List.iter (fun y -> ppStr y) xs
  let rec ppExpr = function
    | ILit(d) -> Printf.fprintf outc "%d" d
    (* | ILit(d) -> Printf.fprintf outc "%o" d (\* octal *\) *)
    (* | ILit(d) -> Printf.fprintf outc "%x" d (\* hex *\) *)
    (* | ILit(d) -> Printf.fprintf outc "%X" d (\* Hex *\) *)
    | FLit(f) -> Printf.fprintf outc "%f" f
    | BLit(b) -> Printf.fprintf outc "%b" b
    | RLit(c) -> Printf.fprintf outc "'%c'" c
    | SLit(s) -> Printf.fprintf outc "\"%s\"" s
    | Iden(x) -> Printf.fprintf outc "%s" x
    | Bexp(op,e1,e2) -> ppStr "("; ppExpr e1; ppStr (op_to_str op); ppExpr e2; ppStr ")"
    | Uexp(op,e) -> ppStr "("; ppExpr e1; ppStr op; ppExpr e2; ppStr ")"

  and printDecl = function
    | Var_decl(xs, eso, typo) ->
       ppStr "var "; pcsl xs; 
       may (fun typ -> ppStr (string_of_typ typ)) typo; 
       may (fun es -> (ppStr " = "); pcsl es) eso; ppStr ";"

    | Slice_decl(xs, typ) ->
       ppStr "var "; pcsl xs; 
       ppStr ("[]"^(string_of_typ typ)); ppStr ";"

    | Array_decl(xs, d, typ) ->
       ppStr "var "; pcsl xs; 
       ppStr (" [" ^ string_of_int d ^ "]"^(string_of_typ typ) ^ ";")

    | Type_decl(x, typ) ->
       ppStr "type "; ppStr x; ppStr " "; ppStr typ; ppStr ";"

    (* | Struct_decl(x, ys, typ) -> *)
    (*    ppStr "type "; ppStr x; ppStr " struct {\n"; *)
    (*    ppStr "var "; ppStr x; ppStr " "; ppStr typ; ppStr ";" *)
    (*    of var_id * var_id list * typ_id *)

    | Func_decl(f, xstLs, ss, yo, typo) ->
       ppStr "func "^f^"(";
       (match xstLs with
        | [] -> ()
        | (xs,t)::tl ->
           pcsl xs; ppStr (string_of_typ t);
           List.iter (fun (xs,t) ->
               ppStr ", "; pcsl xs; ppStr (" "^(string_of_typ t))) tl);
       ppStr ") ";
       may (fun y -> ppStr (y ^ " ")) yo; 
       may (fun typ -> ppStr (string_of_typ typ) ^ " ") typo; 
       ppStr "{\n";
       ppStmts ss;
       ppStr "return";
       may (fun y -> ppStr " "^y) yo; 
       ppStr "\n}";

    | Empty -> ()

  and ppStmt = function
    | Assign(x) -> tab(); ppStr (x ^ " = "); ppExpr e; ppStr ";\n"
      (* of 'e assignment *)
    | Print(x) -> tab(); ppStr "print "; ppExpr e; ppStr ";\n" (* add println ! *)
      (* of 'e *)
    | If_stmt(e,xs,yso) -> 
       Printf.fprintf outc "%tif %t {\n%t}\n%t"
                      (fun c -> tab())
                      (fun c ->  ppExpr e)
                      (fun c -> incr tabCount; List.iter ppStmt xs; may (fun ys -> tabWith(!tabCount-1); ppStr "else\n"; List.iter ppStmt ys) yso) 
                      (fun c -> decr tabCount)

    | For_stmt(eo,ss) ->
       Printf.fprintf outc "%tfor %t {\n%t}\n%t"
                      (fun c -> tab())
                      (fun c -> may (fun e -> ppExpr e) eo)
                      (fun c -> incr tabCount; List.iter ppStmt ss) 
                      (fun c -> decr tabCount)

    | Var_stmt(xs,eso,typo) ->  ()
       Printf.fprintf outc "%tvar %t;\n"
                      (fun c -> tab())
                      (fun c -> pcsl xs;
                                may (fun typ -> ppStr (" "^string_of_typ typ)) eso;
                                may (fun es -> ppStr " = "; pcsl es) eso)

 (* I'm not sure about the ast nodes for the following but I have this so we can debug *)
    | Slice_stmt(xs,v) ->  
       Printf.fprintf outc "%t = append(%t,%t);\n" (* should we assign here or just append?? *)
                      (fun c -> tab(); pcsl xs)
                      (fun c -> ppStr pcsl xs)
                      (fun c -> ppStr v)

    | Array_stmt(xs,d,typ) -> 
       Printf.fprintf outc "%t[%t] = %t;\n" 
                      (fun c -> tab(); ppStr xs)
                      (fun c -> ppStr d)
                      (fun c -> ppStr v)

    | Type_stmt(x,typ) -> 

    | Struct_stmt(x,ys,typ) -> 
       Printf.fprintf outc "not implemented"

    | Empty -> ()

                 (* missing incr and decr? *)

  in
  List.iter printDecl decls; println(); List.iter ppStmt stmts

       (* let rec pfdecls = function *)
       (*   | [] -> () *)
       (*   | [(xs,t)] -> pcsl xs; ppStr (" "^(string_of_typ t)) *)
       (*   | (xs,t)::tl -> pcsl xs; ppStr (" "^(string_of_typ t)^", "); pfdecls tl *)
       (* in *)

       (* List.iter (fun (xs,t) -> pcsl xs; ppStr (" "^(string_of_typ t)^", ")) xstLs; *)

  (* let pcsl ls =  *)
  (*   List.iteri (fun i x -> if i > 0 then ppStr (" ," ^ x) else ppStr x) ls;; *)
