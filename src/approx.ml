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
;;

let transform_rect (t : transformation) (r : rectangle) : rectangle =
  (* On récupère les coins du rectangle en paramètre *)
  let corners = corners r in
  match t with 
  | Translate vector ->
    (* On applique pour chaque coin la translation *)
    let corners_after_translation = List.map (fun x -> translate vector x) corners in
    (* On retourne le rectangle fait avec ces points *)
    rectangle_of_list corners_after_translation
  | Rotate (point, angle) -> 
    (* On applique pour chaque coin la rotation *)
    let corners_after_rotation = List.map (fun x -> rotate point angle x) corners in
    (* On retourne le plus petit rectangle qui contient chaque point de la liste *)
    rectangle_of_list corners_after_rotation

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
