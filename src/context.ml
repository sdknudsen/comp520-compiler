open Ast

(* Cactus stack of hash tables *)
type context =
  | Root of (string, info) Hashtbl.t
  | Frame of (string, info) Hashtbl.t * context
and info = (string, (string * context)) annotated_typ

let rec typ_to_str = function
  | TSimp(x,_) -> x
  | TStruct(id_typ_ls) -> "#struct { "
                        ^ (String.concat 
                            ", "
                            (List.map (fun (id, t) -> id ^ ":" ^ (typ_to_str t)) id_typ_ls)
                          )
                        ^ "}"
  | TArray(t,d) -> "[" ^ (string_of_int d) ^ "]" ^ (typ_to_str t)
  | TSlice(t) -> "[]" ^ (typ_to_str t)
  | TFn(ts,x) -> "("
               ^ (String.concat ", " (List.map typ_to_str ts)) 
               ^ ") -> "
               ^ (typ_to_str x)
  | TKind(t) -> "#kind(" ^ (typ_to_str t) ^ ")"
  | TVoid -> "#void"

let add name kind = function
  | Frame(tbl, _) -> if Hashtbl.mem tbl name
                     then raise (Error.CompileError "Variable Declared")
                     else Hashtbl.add tbl name kind
  | Root(tbl) -> if Hashtbl.mem tbl name
                 then raise (Error.CompileError "Variable Declared")
                 else Hashtbl.add tbl name kind

let init () = Root(Hashtbl.create 1337)

let rec get_scope name g = match g with
  | Frame(tbl, c) -> if Hashtbl.mem tbl name
                     then g
                     else get_scope name c
  | Root(tbl) -> g 

let scope parent_ctx =
  let new_ctx = Frame(Hashtbl.create 1337, parent_ctx) in
  new_ctx

let unscope line outc dumpsymtab = function
  | Frame(tbl, ctx) ->
      let print_symtab tbl =
        (* print hash table contents: reference [7] *)
        Hashtbl.fold
          (fun key value init ->
            Printf.sprintf "%s -> %s" key (typ_to_str value) :: init)
          tbl []
      in
      if dumpsymtab then
        Printf.fprintf outc "Scope exited at line %d:\n%s\n" line
          (String.concat "\n" (print_symtab tbl));
  | Root(tbl) -> raise (Error.CompileError "Cannot exit global scope")

let base_type t = match t with
  | "int" | "float64" | "rune" | "string" | "bool" -> true
  | _ -> false

let in_scope name = function
  | Root(tbl) | Frame(tbl, _) -> 
     base_type name || Hashtbl.mem tbl name

let rec in_context name = function
  | Frame(tbl, tl) -> base_type name || Hashtbl.mem tbl name || in_context name tl
  | Root(tbl)      -> base_type name ||Hashtbl.mem tbl name

let rec find name = function
  | Frame(tbl, ctx) -> if Hashtbl.mem tbl name
                       then Some(Hashtbl.find tbl name)
                       else find name ctx
  | Root(tbl)       -> if Hashtbl.mem tbl name
                       then Some(Hashtbl.find tbl name)
                       else None (*raise (Error.CompileError (Printf.sprintf "Undeclared symbol %s" name))*)

(*
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
                              | _ -> Some(name)
                         else name 
  in inner name None
*)


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
