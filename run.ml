open Error

let _ =
  let write f obj name suff = f obj (open_out (name^suff)) in
  let errorMsg kind p =
    let line = p.Lexing.pos_lnum in
    let col = p.Lexing.pos_cnum - p.Lexing.pos_bol in
    "Invalid - " ^ kind ^ " on line " ^ string_of_int line ^
      ", character " ^ string_of_int col ^ "\n"
  in
  try
    let filename = Sys.argv.(1) in
    let name = Filename.chop_suffix filename ".min" in
    let filein = open_in filename in

    let lexbuf = Lexing.from_channel filein in
    let untypedTree = Parser.main Lexer.lex lexbuf in
    let symTable = Ast.symTable untypedTree in
    let _ = write Pprint.ppTable symTable name ".symbol.txt" in
    let typedTree = Ast.typeAST untypedTree in
    let _ = write Pprint.ppTree typedTree name ".pretty.min" in
    let _ = write Pprint.ppC (typedTree,symTable) name ".c" in
    print_string "Valid\n"

  with Lexer.LexError(p) -> print_string (errorMsg "Lex error" p);
                            flush stdout; exit(-1)
     | Error.ParseError(p,m) -> print_string (errorMsg ("Parse error in "^m) p);
                                flush stdout; exit(-1)
     | Parser.Error -> print_string "Invalid - Parse error\n";
                       flush stdout; exit(-1)
     | Ast.TypeError(m) -> print_string ("Invalid - Type error: "^m^"\n");
                           flush stdout; exit(-1)
     | Ast.DeclError(m) -> print_string ("Invalid - Declaration error: "^m^"\n");
                           flush stdout; exit(-1)
