open Ast

exception ContextError of string

(* Cactus stack of hash tables *)
type context =
  | Root of (string, info) Hashtbl.t
  | Frame of (string, info) Hashtbl.t * context
and info = kind * (string annotated_typ)
and kind = Var | Typ | Fun

let typ_to_str = function
  | TSimp(x) -> x
  | TStruct(id_typ_ls) -> failwith "not done"
  | TArray(t,d) -> failwith "not done"
  | TSlice(t) -> failwith "not done"
  | Void -> failwith "not done"

let add name kind = function
  | Frame(tbl, _) -> if Hashtbl.mem tbl name
                     then raise (ContextError "Variable Declared")
                     else Hashtbl.add tbl name kind
  | Root(tbl) -> if Hashtbl.mem tbl name
                 then raise (ContextError "Variable Declared")
                 else Hashtbl.add tbl name kind

let init () =
  let ctx = Root(Hashtbl.create 1337) in
  add "true"  (Var, (TSimp "bool")) ctx;
  add "false" (Var, (TSimp "bool")) ctx;
  ctx

let scope parent_ctx =
  let new_ctx = Frame(Hashtbl.create 1337, parent_ctx) in
  new_ctx

let typ_to_str = function
  | TSimp(x) -> x
  | TStruct(id_typ_ls) -> failwith "not done"
  | TArray(t,d) -> failwith "not done"
  | TSlice(t) -> failwith "not done"
  | Void -> failwith "not done"


(*
let unscope outc dumpsymtab = function
  | Frame(tbl, parent_ctx) ->
      let kind_to_str = function
        | Var -> "var"
        | Typ -> "type"
        | Fun -> "func"
      in
      let print_symtab tbl =
        (* print hash table contents: reference [7] *)
        Hashtbl.fold
          (fun key value init ->
             let (kind, typ) = value in
            Printf.sprintf "%s (%s)-> %s" key (kind_to_str kind) (typ_to_str typ) :: init)
          tbl []
      in
      if dumpsymtab then
        Printf.fprintf outc "Scope exited:\n%s\n"
          (String.concat "\n" (print_symtab tbl));
      parent_ctx
  | Root -> raise (ContextError "Empty Context")
*)

let in_scope name = function
  | Root(tbl) | Frame(tbl, _) -> Hashtbl.mem tbl name

let rec in_context name = function
  | Frame(tbl, tl) -> Hashtbl.mem tbl name || in_context name tl
  | Root(tbl)      -> Hashtbl.mem tbl name

let rec find name = function
  | Frame(tbl, ctx) -> if Hashtbl.mem tbl name
                       then Hashtbl.find tbl name
                       else find name ctx
  | Root(tbl)       -> if Hashtbl.mem tbl name
                       then Hashtbl.find tbl name
                       else raise (ContextError (Printf.sprintf "Undeclared symbol %s" name))

let rec get_base_type name =
  (* function *)
  let rec inner name ct = match ct with
    | Frame(tbl, ctx) -> if Hashtbl.mem tbl name
                         then match Hashtbl.find tbl name with
                              | (Typ, TSimp(f)) -> inner f ct
                              | _ -> name
                         else inner name ctx
    | Root(tbl)       -> if Hashtbl.mem tbl name
                         then match Hashtbl.find tbl name with
                              | (Typ, TSimp(f)) -> inner f ct
                              | _ -> name
                         else name 
  in inner name



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
                                 *)

                                (*
module Ctx :
sig
  type values = { scp : string; typ : string };;
  type context = (values Frame.t) list
  (* val empty : 'a list *)
                                      (* type context = frame list *)
                                      (* val empty : 'a context *)
end *)
