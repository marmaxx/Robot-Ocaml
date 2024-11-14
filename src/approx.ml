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

(* Fonction très similaire a run dans le fichier interp mais avec des rectangles à la place des points *)
let run_rect (prog : program) (r : rectangle) : rectangle list =
  let unfolded_prog = unfold_repeat prog in
  let rec execute_program prog current_rectangle visited_rectangles =
    match prog with
    | [] -> List.rev visited_rectangles
    | Either (first_prog, second_prog) :: _ -> 
      let random = Random.bool () in 
      if random then execute_program first_prog r visited_rectangles 
      else execute_program second_prog r visited_rectangles
    | Repeat _ :: _ -> failwith "error in unfold_repeat"
    | Move t :: rest ->
        (* On calcule le nouveau rectangle avec la fonction précèdente *)
        let new_rect = transform_rect t current_rectangle in
        execute_program rest new_rect (new_rect :: visited_rectangles)
  in
  execute_program unfolded_prog r [r]

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
