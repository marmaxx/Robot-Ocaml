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
let print_rectangle rect width height =
  fill_rect 
    ((width / 2) + (i rect.x_min))
    ((height / 2) - (i rect.y_min)) 
    ((i rect.x_max) - (i rect.x_min)) 
    ((i rect.y_max) - (i rect.y_min))

(* Dessiner les axes en fonction de la taille de la fenêtre *)
let draw_axis width height =
  moveto 0 (height / 2);
  lineto width (height / 2);
  moveto (width / 2) 0;
  lineto (width / 2) height

(* Dessiner une spirale pour le guidage visuel *)
let draw_spiral width height =
  moveto (width + 150) height;
  lineto (width + 150) (height + 150);
  lineto (width - 150) (height + 150);
  lineto (width - 150) (height - 150);
  lineto (width + 300) (height - 150);
  lineto (width + 300) (height + 300);
  lineto (width - 300) (height + 300);
  lineto (width - 300) (height - 300);
  lineto (width + 450) (height - 300);
  lineto (width + 450) (height + 450);
  lineto (width - 450) (height + 450);
  lineto (width - 450) (height - 450);
  lineto (width + 450) (height - 450)

(* Impression des états d'un programme en fonction de la taille de la fenêtre *)
let rec print_rectangles rects width height = 
  match rects with
  | [] -> ()
  | h :: t -> 
      print_rectangle h width height;
      Unix.sleep 1;
      clear_graph ();
      draw_axis width height;
      draw_spiral 450 450; (* Guide visuel pour ma spirale, à enlever après ou à séparer pour l'exécution du programme spirale *)
      print_rectangles t width height

(* Impression d'un point *)
let print_point point width height =
  plot ((width / 2) + (i point.x)) ((height / 2) - (i point.y))

(* Impression de la liste de points du robot en fonction de la taille de la fenêtre *)
let rec print_points points width height = 
  match points with
  | [] -> ()
  | h :: t -> 
      print_point h width height;
      Unix.sleep 1;
      clear_graph ();
      draw_axis width height;
      draw_spiral 450 450; (* Guide visuel pour ma spirale, à enlever après ou à séparer pour l'exécution du programme spirale *)
      print_points t width height

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
  draw_axis 900 900;
  print_rectangles list_positions_prog_2 900 900;
  print_points list_positions_prog_1_point 900 900;
  print_rectangles list_positions_prog_1 900 900;
  ignore (read_line ()); (* Permet que la fenêtre ne se ferme pas toute seule, elle attend que l'utilisateur appuie sur Entrée dans le terminal *)
  close_graph ()
