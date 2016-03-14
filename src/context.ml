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
