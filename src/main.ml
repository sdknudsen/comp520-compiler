open Tokens
open Context

(* utilities *)
let write f obj name suff = f obj (open_out (name^suff))
let dumpsymtab = ref false
let smartsymtab = ref false
let pptype = ref false
let file = ref "out.go"

(* actions *)
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

(*
let pretty lexbuf =
  let name = Filename.chop_suffix !file ".go" in
  let untypedTree = Parser.program Lexer.token lexbuf in
  write Pprint.pTree untypedTree name ".pretty.go"
  (* Pprint.pTree untypedTree stdout *)
*)

let typecheck lexbuf =
  let name = (Filename.chop_extension !file) in
  if !dumpsymtab
  then Context.dumbout := Some(open_out (name^".symtab"));
  if !smartsymtab
  then Context.smartout := Some(open_out (name^".smartsymtab"));
  let untypedTree = Parser.program Lexer.token lexbuf in
  Weed.weed untypedTree;
  let typedTree,_ = Type.typeAST untypedTree in

  (if !pptype then
    write Pprint.ptTree typedTree name ".pptype.go"
  else
    ignore(typedTree));

  print_endline "Valid"

let compile lexbuf =
  let name = (Filename.chop_extension !file) in
  if !dumpsymtab
  then Context.dumbout := Some(open_out (name^".symtab"));
  if !smartsymtab
  then Context.smartout := Some(open_out (name^".smartsymtab"));
  let untypedTree = Parser.program Lexer.token lexbuf in
  let _ = Weed.weed untypedTree in
  let (typedTree,indexTable) = Type.typeAST untypedTree in
  if !pptype
  then write Pprint.ptTree typedTree name ".pptype.go";
  write (Gen.generate indexTable) typedTree name ".wast";
  
  if !dumpsymtab
  then Printf.printf "dumpsymtab generated: %s.symtab\n" name;
  if !smartsymtab
  then Printf.printf "smartsymtab generated: %s.smartsymtab\n" name;
  if !pptype
  then Printf.printf "pptype generated: %s.pptype.go\n" name;
  Printf.printf "WebAssembly code generated: %s.wast\n" name

let main =
  (* command line arguments with flags: reference [8] *)
  let speclist =
    [("-dumpsymtab",
        Arg.Set dumpsymtab,
        "Enables top-most frame of the symbol table to be dumped each time a scope is exited");
     ("-smartsymtab",
        Arg.Set smartsymtab,
        "Enables a smarter symbol table with lexical based indentation");
     ("-pptype",
        Arg.Set pptype,
        "Enables pretty print of the program, with the type of each expression printed in some legible format")]
  in
  let usage_msg = "<path_to_src>/main.native [lex|parse|weed|pretty|type|compile] [-dumpsymtab] [-pptype] [<path_to_programs>/foo/bar.go]" in
  (*[-dumpsymtab|-pptype]*)(* add -dumpsymtabll? *)
  let action = ref compile in
  let in_channel = ref stdin in
  (*let file = ref "foo/bar.go" in*)
  let anon_fun = function
    | "lex"       -> action := lex
    | "parse"     -> action := parse
    | "weed"      -> action := weed
(*
    | "pretty"    -> action := pretty
*)
    | "type" -> action := typecheck

    | "compile"   -> action := compile
    (* unknown arguments are considered as files *)
    | _ as f    -> (in_channel := open_in f;
                    file := f)
  in
  begin
    Arg.parse speclist anon_fun usage_msg;
    let lexbuf = Lexing.from_channel !in_channel in
    try
      !action lexbuf
    with
      | Error.CompileError message ->
         Printf.eprintf "Invalid %s\n" message;
         exit 1
      | Parser.Error ->
         Printf.eprintf "Invalid %s"
           (Error.print_error lexbuf.Lexing.lex_curr_p "syntax error");
         exit 1
  end

let _ = main
