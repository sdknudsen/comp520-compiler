let _ =
  (* let write f obj name suff = f obj (open_out (name ^ suff)) in *)
  let filename = Sys.argv.(1) in
  (* let name = Filename.chop_suffix filename ".go" in *)
  let filein = open_in filename in
  let lexbuf = Lexing.from_channel filein in
  try
    let untypedTree = Parser.program Lexer.token lexbuf in
    (* let typedTree = Ast.typeAST untypedTree in
    let _ = write Pprint.ppTree typedTree name ".pretty.go" in *)
    ignore (untypedTree);
    print_endline "Valid"
  with
    | Error.CompileError message -> print_endline ("Invalid" ^ message)
    | Parser.Error -> print_endline
      ("Invalid" ^ (Error.print_error lexbuf.Lexing.lex_curr_p "syntax error"))
