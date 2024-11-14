open Geo
open Interp

(* Code de la Section 5 du projet. *)

let sample (rect : rectangle) : point =
  Random.self_init (); (* On initialise un générateur *)
  (* On choisit une abscisse entre rect.x_min et rect.x_max *)
  let x = rect.x_min +. Random.float (rect.x_max -. rect.x_min) in 
  (* On choisit une ordonnée entre rect.y_min et rect.y_max *)
  let y = rect.y_min +. Random.float (rect.y_max -. rect.y_min) in
  {x ; y}
  
let transform_rect (t : transformation) (r : rectangle) : rectangle =
  failwith "À compléter"

let run_rect (prog : program) (r : rectangle) : rectangle list =
  failwith "À compléter"

let inclusion (r : rectangle) (t : rectangle) : bool =
  failwith "À compléter"

let target_reached_rect (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"

let run_polymorphe (transform : transformation -> 'a -> 'a) (prog : program) (i : 'a) : 'a list =
  failwith "À compléter"

let rec over_approximate (prog : program) (r : rectangle) : rectangle =
  failwith "À compléter"

let feasible_target_reached (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"
