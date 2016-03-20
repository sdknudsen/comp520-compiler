open Tokens

let lex lexbuf =
  let rec consume lb =
    let t = Lexer.token lb in
    match t with
     | EOF    -> (print_endline (Lexer.print_token EOF))
     | _      -> (print_endline (Lexer.print_token t);
                 consume lb)
  in consume lexbuf



let parse lexbuf =
  let untypedTree = Parser.program Lexer.token lexbuf in
  ignore (untypedTree);
  print_endline "Valid"


let weed lexbuf =
  let untypedTree = Parser.program Lexer.token lexbuf in
  Weed.weed untypedTree;
  print_endline "Valid"

let pretty lexbuf =
  let untypedTree = Parser.program Lexer.token lexbuf in
  Pprint.pTree untypedTree stdout


let typecheck lexbuf =
  let untypedTree = Parser.program Lexer.token lexbuf in
  let typedTree = Type.typeAST untypedTree in
  (* let _ = write Pprint.ppTree typedTree name ".pretty.go" in *)
  ignore (typedTree);
  print_endline "Valid"

let compile lexbup =
  print_endline "Compiling is complicated"

let main =
let usage_msg = "golite [lex|parse|pretty|typecheck|compile] [<file>]"
(*[-dumpsymtab|-pptype]*)(* add -dumpsymtabll? *)
in let action = ref compile
in let in_channel = ref stdin
in let anon_fn str =
  match str with
    | "lex"       -> action := lex
    | "pretty"    -> action := pretty
    | "parse"     -> action := parse
    | "weed"      -> action := weed
    | "typecheck" -> action := typecheck
    | "compile"   -> action := compile
    (* unknown arguments are considered as files *)
    | _ as f    -> in_channel := open_in f
in begin
  Arg.parse [] anon_fn usage_msg;
  let lexbuf = Lexing.from_channel !in_channel in
  try
    !action lexbuf;
  with
    | Error.CompileError message ->
       Printf.fprintf stderr "Invalid %s\n" message;
       exit 1
    | Parser.Error ->
       Printf.fprintf 
         stderr
         "Invalid %s"
         (Error.print_error lexbuf.Lexing.lex_curr_p "syntax error")
       ;
       exit 1
end

let _ = main
