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
       may (fun typ -> ppStr (typ_to_string typ)) typo; 
       may (fun es -> (ppStr " = "); pcsl es) eso; ppStr ";"

    | Slice_decl(xs, typ) ->
       ppStr "var "; pcsl xs; 
       ppStr ("[]"^(typ_to_string typ)); ppStr ";"

    | Array_decl(xs, d, typ) ->
       ppStr "var "; pcsl xs; 
       ppStr (" [" ^ string_of_int d ^ "]"^(typ_to_string typ) ^ ";")

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
           pcsl xs; ppStr (typ_to_string t);
           List.iter (fun (xs,t) ->
               ppStr ", "; pcsl xs; ppStr (" "^(typ_to_string t))) tl);
       ppStr ") ";
       may (fun y -> ppStr (y ^ " ")) yo; 
       may (fun typ -> ppStr (typ_to_string typ) ^ " ") typo; 
       ppStr "{\n";
       ppStmts ss;
       ppStr "return";
       may (fun y -> ppStr " "^y) yo; 
       ppStr "\n}";

    | Empty -> ()

  and ppStmt = function
    | Assign(x) -> ()
      (* of 'e assignment *)
    | Print(x) -> ()
      (* of 'e *)
    | If_stmt(x) -> ()
      (* of 'e * 's list * 's list option  *)
    | For_stmt(x) -> ()
      (* of ('e * ('e assignment * 'e assignment) option) option * 's list *)
    | Var_stmt(x) -> ()
      (* of var_id list * 'e list option * typ_id option *)
    | Slice_stmt(x) -> ()
      (* of var_id list * typ_id *)
    | Array_stmt(x) -> ()
      (* of var_id list * int * typ_id *)
    | Type_stmt(x) -> ()
      (* of var_id * typ_id *)
    | Struct_stmt(x) -> ()
      (* of var_id * var_id list * typ_id *)
  | Empty -> ()

    | Assign(id,e) -> tab(); ppStr (id ^ " = "); ppExpr e; ppStr ";\n" (* *)
    | Print(e) -> tab(); ppStr "print "; ppExpr e; ppStr ";\n"
    | Read(id) -> tab(); ppStr "read "; ppStr id; ppStr ";\n"

    | Ifte(e,xs,ys) ->
       tab(); ppStr "if "; ppExpr e; ppStr " then\n"; incr tabCount;
       List.iter ppStmt xs; tabWith(!tabCount-1); ppStr "else\n";
       List.iter ppStmt ys; decr tabCount;
       tab(); ppStr "endif\n"

    | Ift(e,xs) ->
       tab(); ppStr "if "; ppExpr e; ppStr " then\n"; incr tabCount;
       List.iter ppStmt xs; decr tabCount; tab(); ppStr "endif\n"

    | While(e,xs) ->
       tab(); ppStr "while "; ppExpr e; ppStr " do\n"; incr tabCount;
       List.iter ppStmt xs; decr tabCount; tab(); ppStr "done\n"
  in
  List.iter printDecl decls; println(); List.iter ppStmt stmts

       (* let rec pfdecls = function *)
       (*   | [] -> () *)
       (*   | [(xs,t)] -> pcsl xs; ppStr (" "^(typ_to_string t)) *)
       (*   | (xs,t)::tl -> pcsl xs; ppStr (" "^(typ_to_string t)^", "); pfdecls tl *)
       (* in *)

       (* List.iter (fun (xs,t) -> pcsl xs; ppStr (" "^(typ_to_string t)^", ")) xstLs; *)

  (* let pcsl ls =  *)
  (*   List.iteri (fun i x -> if i > 0 then ppStr (" ," ^ x) else ppStr x) ls;; *)
