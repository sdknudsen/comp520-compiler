open Ast
open Ho

let symout = ref None

let set_outfile oc = 
  symout := Some(oc)

(* Cactus stack of hash tables *)
type context =
  | Root of (string, info) Hashtbl.t * int * string
  | Frame of (string, info) Hashtbl.t * context * int * string
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
  | TKind(t) -> "kind(" ^ (typ_to_str t) ^ ")"
  | TVoid -> "#void"

let rec scope_depth  = function
  | Frame(_,_,depth,_) | Root(_,depth,_) -> depth

let add name kind ctx =
  (match ctx with
  | Frame(tbl,_,_,_) ->
     if Hashtbl.mem tbl name
     then raise (Error.CompileError ("Symbol "^name^" already in use"))
     else Hashtbl.add tbl name kind
  | Root(tbl,_,_) ->
     if Hashtbl.mem tbl name
     then raise (Error.CompileError ("Symbol "^name^" already in use"))
     else Hashtbl.add tbl name kind);
  may (fun o ->
           Printf.fprintf
             o 
             "%s%s -> %s\n"
             (String.init ((scope_depth ctx) * 4 + 2) (fun x -> ' '))
             name
             (typ_to_str kind))
      !symout
  
let init () = let r = Root(Hashtbl.create 1337, 0, "a") in
  may (fun o -> Printf.fprintf o "Symbol table\n================\n\n") !symout;
  r

let rec get_scope name g = match g with
  | Frame(tbl,c,_,_) ->
      if Hashtbl.mem tbl name
      then g
      else get_scope name c
  | Root(tbl,_,_) -> g 


let indent n = 
  may (fun o -> Printf.fprintf o
                  "%s"
                  (String.init (n * 4) (fun x -> ' ')))
      !symout

let scope parent_ctx =
  let new_ctx = Frame(Hashtbl.create 1337,
                      parent_ctx,
                      (scope_depth parent_ctx) + 1,
                      "b")
  in
  may (fun o ->
         let depth = scope_depth new_ctx in
         indent depth;
         Printf.fprintf o "{\n")
      !symout;
  new_ctx

(*
let print_scope ctx = (match (!symout,ctx) with
  | Some(outf), Frame(tbl,_,_,_)
  | Some(outf), Root(tbl,_,_) ->
      let depth = scope_depth ctx in
      Hashtbl.iter
        (fun key value ->
           let typ = value in
           Printf.fprintf
             outf
             "%s%s -> %s\n"
             (String.init (depth * 4 + 2) (fun x -> ' '))
             key
             (typ_to_str typ))
        tbl
      ;
  | _ -> ())
*)

let unscope g = begin
  may (fun o ->
         let depth = scope_depth g in
         indent depth;
         Printf.fprintf o "}\n")
      !symout
end

let in_scope name = (function
  | Root(tbl,_,_) | Frame(tbl,_,_,_) -> 
     Hashtbl.mem tbl name)

let rec get_type_instance name c = match c with
  | Frame(tbl,ctx,_,_) ->
      if Hashtbl.mem tbl name
      then match Hashtbl.find tbl name with
       | TKind(_) -> Some(TSimp(name, c))
       | _ -> None
      else get_type_instance name ctx
  | Root(tbl,_,_) ->
      if Hashtbl.mem tbl name
      then match Hashtbl.find tbl name with
       | TKind(_) -> Some(TSimp(name, c))
       | _ -> None
      else None

let rec in_context name = (function
  | Frame(tbl,tl,_,_) ->
      Hashtbl.mem tbl name || in_context name tl
  | Root(tbl,_,_) ->
      Hashtbl.mem tbl name)

let rec find name = (function
  | Frame(tbl,ctx,_,_) ->
      if Hashtbl.mem tbl name
      then Some(Hashtbl.find tbl name)
      else find name ctx
  | Root(tbl,_,_) ->
      if Hashtbl.mem tbl name
      then Some(Hashtbl.find tbl name)
      else None)
