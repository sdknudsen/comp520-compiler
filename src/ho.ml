(* Higher order function (mostly) *)

let iso = function
  | Some(_) -> true
  | None -> false

let sure = function
  | Some(x) -> x
  | None -> failwith "So sure?\n" 

let defaulto f v = function 
  | Some(x) -> f x
  | None -> v

let mapo f = function
  | Some(x) -> Some(f x)
  | None -> None

let may f = function
  | Some(x) -> f x;
  | None -> ()

let compose f g = fun (x) -> g (f x)

