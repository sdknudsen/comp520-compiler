exception EmptyContext
(* use a map instead of a hash table? *)
(* module Ctx = Map.Make(String) *)
(* I'm not sure how correct everything is *)

module Ctx = 
      struct
        module Frame = Map.Make(String)
        type values = { scp : string; typ : string };;
        type context = (values Frame.t) list

        let create () = [Frame.empty]

        let scope stack = Frame.empty::stack

        let unscope = function
          | hd::tl -> tl
          | _ -> raise EmptyContext

        let find stack = Frame.empty::stack

        let is_empty = function
          | [x] -> Frame.is_empty x
          | x::xs -> false
          | [] -> raise EmptyContext

        let rec mem k g = List.exists (fun x -> Frame.mem k x) g

        let add k v g = match g with
          | x::xs -> Frame.add 
          | [] -> raise EmptyContext

        let find k v g = match g with
          | x::xs -> Frame.find  
          | [] -> raise EmptyContext

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
