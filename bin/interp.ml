open Pf5
open Geo
open Approx
open Interp
open Graphics

(* Pour lancer le programme :

   dune build (compile)
   dune exec view (execute)

*)

(* Fonction qui crée un rectangle avec les coordonnées passées *)
let rectangle x_min x_max y_min y_max = 
  { x_min; x_max; y_min; y_max }

(* Fonction qui crée un point avec les coordonnées passées *)
let point x y =
  { x; y }

(* Fonction pour éviter d'écrire int_of_float à chaque fois *)
let i x = int_of_float x

(* Impression d'un rectangle simple avec coordonnées ajustées par rapport à la taille de la fenêtre *)
let print_rectangle rect width height rect_colour =
  set_color rect_colour;
  fill_rect 
    ((width / 2) + (i rect.x_min))
    ((height / 2) - (i rect.y_min)) 
    ((i rect.x_max) - (i rect.x_min)) 
    ((i rect.y_max) - (i rect.y_min))

(* Dessiner les axes en fonction de la taille de la fenêtre *)
let draw_axis width height axis_colour =
  set_color axis_colour;
  moveto 0 (height / 2);
  lineto width (height / 2);
  moveto (width / 2) 0;
  lineto (width / 2) height

let fill_background width height background_colour =
  set_color background_colour;
  fill_rect 0 0 width height

(* Impression des états d'un programme en fonction de la taille de la fenêtre *)
let rec print_rectangles rects width height axis_colour background_colour rect_colour = 
  match rects with
  | [] -> ()
  | h :: t -> 
      print_rectangle h width height rect_colour;
      Unix.sleep 1;
      clear_graph ();
      fill_background width height background_colour;
      draw_axis width height axis_colour;
      print_rectangles t width height axis_colour background_colour rect_colour

(* Impression d'un point *)
let print_point point width height point_colour =
  set_color point_colour;
  fill_circle ((width / 2) + (i point.x)) ((height / 2) - (i point.y)) 4

(* Impression de la liste de points du robot en fonction de la taille de la fenêtre *)
let rec print_points points width height axis_colour background_colour point_colour = 
  match points with
  | [] -> ()
  | h :: t -> 
      print_point h width height point_colour;
      Unix.sleep 1;
      clear_graph ();
      fill_background width height background_colour;
      draw_axis width height axis_colour;
      print_points t width height axis_colour background_colour point_colour

(* impression rectangle et point en simultané *)
let print_point_rect rect width height rect_colour point_colour =
  set_color rect_colour;
  fill_rect 
    ((width / 2) + (i rect.x_min))
    ((height / 2) - (i rect.y_min)) 
    ((i rect.x_max) - (i rect.x_min)) 
    ((i rect.y_max) - (i rect.y_min));
  set_color point_colour;
  let central_point = point rect.x_min rect.y_min in
  fill_circle 
    ((width / 2) + int_of_float central_point.x) 
    ((height / 2) - int_of_float central_point.y) 
    4

(* itération pour dérouler le programme avec point + rectangle*)
let rec print_points_rects rects width height axis_colour background_colour rect_colour point_colour= 
  match rects with
  | [] -> ()
  | h :: t -> 
      print_point_rect h width height rect_colour point_colour;
      Unix.sleep 1;
      clear_graph ();
      fill_background width height background_colour;
      draw_axis width height axis_colour;
      print_points_rects t width height axis_colour background_colour rect_colour point_colour

(* Création d'une spirale hyper classe *)
let create_spiral width height =
  let float_width = float_of_int width in
  let float_height = float_of_int height in
  let rect_width = float_width /. 30. in
  let x_unit = rect_width /. 2. in
  let y_unit = float_height /. 60. in
  let up = Move (Translate { x = 0.; y = (-.y_unit) }) in
  let right = Move (Translate { x = x_unit; y = 0. }) in
  let down = Move (Translate { x = 0.; y = y_unit }) in
  let left = Move (Translate { x = (-.x_unit); y = 0. }) in

  [
    Repeat (8, [right]);
    Repeat (9, [up]);
    Repeat (18, [left]);
    Repeat (19, [down]);
    Repeat (28, [right]);
    Repeat (29, [up]);
    Repeat (38, [left]);
    Repeat (39, [down]);
    Repeat (48, [right]);
    Repeat (49, [up]);
    Repeat (58, [left]);
    Repeat (59, [down]);
    Repeat (58, [right])
  ]

  let create_undeterministic_program width height =
    let float_width = float_of_int width in
    let float_height = float_of_int height in
    let rect_width = float_width /. 30. in
    let x_unit = rect_width /. 2. in
    let y_unit = float_height /. 60. in
    let up = Move (Translate { x = 0.; y = (-.y_unit) }) in
    let right = Move (Translate { x = x_unit; y = 0. }) in
    let down = Move (Translate { x = 0.; y = y_unit }) in
    let left = Move (Translate { x = (-.x_unit); y = 0. }) in
    let rot1 = Move (Rotate ({x = 0.; y = 0.}, 90.)) in
    [
      Either ([up], [down]) ; rot1 ; Either ([left], [right]) ; rot1
    ]

(* Création d'un rectangle pour la spirale *)
let create_rectangle_spiral width =
  let float_width = float_of_int width in
  let rect_width = float_width /. 30. in
  let rect_height = rect_width /. 2. in
  rectangle 0. rect_width 0. rect_height

(* Programmes de spirale *)
let spiral_program = create_spiral 900 900
let spiral_rect = create_rectangle_spiral 900
let list_positions_prog_1 = run_rect spiral_program spiral_rect
let list_positions_prog_1_point = run spiral_program (point 0. 0.)

let undeterministic_program = create_undeterministic_program 900 900
let list_positions_prog_2 = run_rect undeterministic_program spiral_rect

(* Gestion de la fenêtre graphique *) 
let _ = 
  (* Pour l'instant la hauteur et largeur de la fenêtre sont paramétrées manuellement, 
     mais après on récupère ça par les options *)
  open_graph " 900x900";
  
  let circle_colour = rgb 156 167 207 in
  let background_colour = rgb 255 229 153 in
  let axis_colour = rgb 153 0 0 in
  let rect_colour = rgb 7 55 99 in
  fill_background 900 900 background_colour;
  draw_axis 900 900 axis_colour;
  print_points_rects list_positions_prog_2 900 900 axis_colour background_colour rect_colour circle_colour;
  print_rectangles list_positions_prog_2 900 900 axis_colour background_colour rect_colour;
  print_points list_positions_prog_1_point 900 900 axis_colour background_colour circle_colour;
  print_rectangles list_positions_prog_1 900 900 axis_colour background_colour rect_colour;
  ignore (read_line ()); (* Permet que la fenêtre ne se ferme pas toute seule, elle attend que l'utilisateur appuie sur Entrée dans le terminal *)
  close_graph ()
