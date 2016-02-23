open Tokens

let lex in_channel =
  let lexbuf = Lexing.from_channel in_channel in
  try
    let rec consume lb =
      let t = Lexer.token lb in
      match t with
       | EOF    -> (print_endline (Lexer.print_token EOF))
       | _      -> (print_endline (Lexer.print_token t);
                   consume lb)
    in consume lexbuf
  with
    | Error.CompileError message ->
        print_endline ("Invalid" ^ message);
        exit 1


let parse in_channel =
  let lexbuf = Lexing.from_channel in_channel in
  try
     let untypedTree = Parser.program Lexer.token lexbuf in
     (* let typedTree = Ast.typeAST untypedTree in
     let _ = write Pprint.ppTree typedTree name ".pretty.go" in *)
     ignore (untypedTree);
     print_endline "Valid"
  with
    | Error.CompileError message ->
        print_endline ("Invalid" ^ message);
        exit 1
    | Parser.Error ->
        print_endline
          ("Invalid" ^ (Error.print_error lexbuf.Lexing.lex_curr_p "syntax error"));
        exit 1


let pretty    in_channel = print_endline "I'm pretty"
let typecheck in_channel = print_endline "Typical typechecker"
let compile   in_channel = print_endline "Compiling is complicated"


(*
let compile in_channel =

*)


let main =
let usage_msg = "golite [lex|parse|pretty|typecheck|compile] [<file>]"
in let action = ref compile
in let in_channel = ref stdin
in let anon_fn str =
  match str with
    | "lex"       -> action := lex
    | "pretty"    -> action := pretty
    | "parse"     -> action := parse
    | "typecheck" -> action := typecheck
    | "compile"   -> action := compile
    (* unknown arguments are considered as a file *)
    | _ as f    -> in_channel := open_in f
in begin
  Arg.parse [] anon_fn usage_msg;
  !action !in_channel;
end

let _ = main
