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
    | Either (first_prog, second_prog) :: rest -> 
      let random = Random.bool () in 
      let chosen_prog = if random then first_prog else second_prog in
      execute_program (chosen_prog @ rest) current_rectangle visited_rectangles
    | Repeat _ :: _ -> failwith "error in unfold_repeat run_rect"
    | Move t :: rest ->
        (* On calcule le nouveau rectangle avec la fonction précèdente *)
        let new_rect = transform_rect t current_rectangle in
        execute_program rest new_rect (new_rect :: visited_rectangles)
  in
  execute_program unfolded_prog r [r]

let inclusion (r : rectangle) (t : rectangle) : bool =
  (* On vérifie pour chaque coin du premier rectangle s'il est dans le deuxième *)
  List.for_all (fun p -> in_rectangle t p) (corners r)

let target_reached_rect (prog : program) (r : rectangle) (target : rectangle) : bool =
  let list_of_rect = run_rect prog r in
  let last_rect =
    match List.rev list_of_rect with
      | [] -> r
      | x :: _ -> x
    in
  inclusion last_rect target

let run_polymorphe (transform : transformation -> 'a -> 'a) (prog : program) (i : 'a) : 'a list =
  let unfolded_prog = unfold_repeat prog in
  let rec execute_program prog current_object visited_objects =
    match prog with
    | [] -> List.rev visited_objects
    | Either (first_prog, second_prog) :: rest -> 
      let random = Random.bool () in 
      let chosen_prog = if random then first_prog else second_prog in
      execute_program (chosen_prog @ rest) current_object visited_objects
    | Repeat _ :: _ -> failwith "error in unfold_repeat run polymorphe"
    | Move t :: rest ->
        let new_obj = transform t current_object in
        execute_program rest new_obj (new_obj :: visited_objects)
  in
  execute_program unfolded_prog i [i]

let rec over_approximate (prog : program) (r : rectangle) : rectangle =
  let unfolded_prog = unfold_repeat prog in
  let rec execute_program prog current_rectangle approx_rectangle =
    match prog with
    | [] -> approx_rectangle
    | Either (first_prog, second_prog) :: rest -> 
      (* executer premier et deuxieme et renvoyer rectangle contenant les 2 sorties *)
      let first_rect = execute_program first_prog current_rectangle approx_rectangle
      in let second_rect = execute_program second_prog current_rectangle approx_rectangle
      in let new_approx_rect = rectangle_of_list (corners first_rect @ corners second_rect)
      in execute_program rest new_approx_rect new_approx_rect
    | Repeat _ :: _ -> failwith "error in unfold_repeat over approximate"
    | Move t :: rest ->
        let new_rect = transform_rect t approx_rectangle in
        execute_program rest new_rect new_rect
  in
  execute_program unfolded_prog r r

let feasible_target_reached (prog : program) (r : rectangle) (target : rectangle) : bool =
  let final_rect = over_approximate prog r 
  in inclusion final_rect target

 
