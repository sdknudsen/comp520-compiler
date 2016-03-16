open Ast

exception ContextError of string

type context = CactusStack of (string, typ) Hashtbl.t * context option

let scope ctx =
  let new_ctx = CactusStack(Hashtbl.create 1337, Some ctx) in
  new_ctx

let unscope ctx outc dumpsymtab =
  let print_symtab ctx =
    Hashtbl.fold
      (fun key value init ->
        Printf.sprintf "%s -> %s" key value :: init)
      ctx []
  in
  if dumpsymtab then
    Printf.fprintf outc "Scope exited:\n%s\n" (String.concat "\n" (print_symtab ctx))

let add name kind = function
  | CactusStack(tbl, _) -> Hashtbl.add tbl name kind

let find name = function
  | CactusStack(tbl, _) -> Hashtbl.find tbl name

let mem name = function
  | CactusStack(tbl, _) -> Hashtbl.mem tbl name

let build_symtab (Prog(_,decls)) outc dumpsymtab = 
  let ctx = CactusStack(Hashtbl.create 1337, None) in
  add "true" (TSimp "bool") ctx;
  add "false" (TSimp "bool") ctx;
  
  let walk_ast = function
    | Var_decl(ids_eso_typo_ls) ->
      List.iter (fun (ids,eso,typo) ->
        ()
        ) ids_eso_typo_ls
    | Type_decl(typId_typ_ls) ->
      List.iter (fun (id,typ) ->
        ()
        ) typId_typ_ls

    | Func_decl(fId, id_typ_ls, typ, ps) ->
      ()
  in
  
  List.iter walk_ast decls;
  ctx



(*
exception ContextError of string
(* use a map instead of a hash table? *)
(* module Ctx = Map.Make(String) *)
(* I'm not sure how correct everything is *)

module Ctx = 
      struct
        module Frame = Map.Make(String)
        type values = { scp : string; typ : string };;
        type context = (values Frame.t) list

        let create () = [Frame.empty] (* can we do this without unit? *)

        let scope stack = Frame.empty::stack

        let unscope stack = try List.tail stack
                            with _ -> raise ContextError "Empty Context"

        let find g = Frame.empty::g

        let is_empty g = List.for_all (fun x -> Frame.is_empty x) g

        let mem k g = List.exists (fun x -> Frame.mem k x) g

        let in_scope k g = match g with
          | [] -> false
          | x::_ -> Frame.mem k x

        let add k v g = try Frame.add k v (List.head g)
                        with _ -> raise ContextError "Empty Context"

        let find k v g = match g with
          | x::xs -> if Frame.mem k x
                     then Frame.find k x
                     else find k v xs
          | [] -> raise ContextError "Undeclared Variable"

        (* let remove (\* remove the first instance found? *\) *)
        (* let equal *)
        (* let iter *)
        (* let fold *)
        (* let for_all *)
        (* let exists *)
        (* let filter *)
        (* let map *)
        end
(*
module Ctx :
sig
  type values = { scp : string; typ : string };;
  type context = (values Frame.t) list
  (* val empty : 'a list *)
                                      (* type context = frame list *)
                                      (* val empty : 'a context *)
end
 *)
*)