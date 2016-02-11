(* Open the ocamlbuild world... *)
open Ocamlbuild_plugin;;

Options.use_menhir:= true;

 (* This dispatch call allows to control the execution order of your
    directives. *)
dispatch begin function
 (* Add our rules after the standard ones. *)
  | After_rules ->
    flag ["only_token"] (S [A "--base";A "tokens"; A "--only-tokens"]);
    flag_and_dep ["external_token"] (S [A "--base";A "parser"; A "--external-tokens"; A "Tokens"; P "tokens.mly"]);
  | _ -> ()
end;;
