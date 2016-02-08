open Lexing

exception CompileError of string

let print_error lexpos err =
  let line = lexpos.pos_lnum in
  let col = lexpos.pos_cnum - lexpos.pos_bol in
  let message = Printf.sprintf " on line %d, character %d %d %d: %s" line col err in
  raise (CompileError message)
