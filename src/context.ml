open Ast

exception ContextError of string

(* Cactus stack of hash tables *)
type context = Root | Frame of (string, typ) Hashtbl.t * context

let init () =
  let ctx = Frame(Hashtbl.create 1337, Root) in
  add "true" (TSimp "bool") ctx;
  add "false" (TSimp "bool") ctx;
  ctx

let scope parent_ctx =
  let new_ctx = Frame(Hashtbl.create 1337, parent_ctx) in
  new_ctx

let unscope ctx outc dumpsymtab =
  let print_symtab ctx =
    Hashtbl.fold
      (fun key value init ->
        Printf.sprintf "%s -> %s" key value :: init)
      ctx []
  in
  if dumpsymtab then
    Printf.fprintf outc "Scope exited:\n%s\n"
      (String.concat "\n" (print_symtab ctx))

let in_scope name = function
  | Frame(tbl, _) -> Hashtbl.mem tbl name
  | Root -> raise (ContextError "Empty Context")

let add name kind = function
  | Frame(tbl, _) -> if Hashtbl.mem tbl name
                     then raise (ContextError "Variable Declared")
                     else Hashtbl.add tbl name kind
  | Root -> raise (ContextError "Empty Context")

let rec find name = function
  | Frame(tbl, ctx) -> if Hashtbl.mem tbl name
                       then Hashtbl.find tbl name
                       else find name ctx
  | Root -> raise (ContextError "Undeclared Variable")



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