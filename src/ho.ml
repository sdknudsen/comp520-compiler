(* Higher order function (mostly) *)

let sure = function
  | Some(x) -> x
  | None -> failwith "So sure?\n" 

let mapo f = function
  | Some(x) -> Some(f x)
  | None -> None

let may f = function
  | Some(x) -> f x;
  | None -> ()

let compose f g = fun (x) -> g (f x)

