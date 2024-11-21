open Geo
open Approx
open Interp
open Graphics

(*pour lancer le programme :

  dune build (compile)
  dune exec view (execute)

*)
(* fonction qui crée un rectangle avec les coordonnées passées *)
let rectangle x_min x_max y_min y_max = 
  { x_min; x_max; y_min; y_max }

(* fonction pour eviter d'écrire int_of_float à chaque fois *)
let i x = int_of_float x

(* impression d'un rectangle simple avec coordonnees ajustees par rapport a la taille de la fenetre*)
let print_rectangle rect width height =
  fill_rect 
  ((width / 2) + (i rect.x_min))
  ((height / 2) - (i rect.y_min)) 
  ((i rect.x_max) - (i rect.x_min)) 
  ((i rect.y_max) - (i rect.y_min))

(* dessiner les axes en fonction de la taille de la fenetre*)
let draw_axis width height =
  moveto 0 (height / 2);
  lineto width (height / 2);
  moveto (width / 2) 0;
  lineto (width / 2) height

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

(* impression des états d'un programme en fonction de la taille de la fenetre *)
let rec print_rectangles rects width height = 
  match rects with
  | [] -> ()
  | h :: t -> 
      print_rectangle h width height;
      Unix.sleep 1;
      clear_graph ();
      draw_axis width height;
      draw_spiral 450 450; (* guide visuel pour ma spirale, à enlever après ou à séparer pour execution du programme spirale*)
      print_rectangles t width height

(* j'ai voulu faire un programme qui fait une spirale hyper classe mais c'est giga chiant avec la manière dont sont imlplémentées les rotations du rectangle*)
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

let create_rectangle_spiral width =
  let float_width = float_of_int width in
  let rect_width = float_width /. 30. in
  let rect_height = rect_width /. 2. in
  rectangle 0. rect_width 0. rect_height

let spiral_program = create_spiral 900 900 
let spiral_rect = create_rectangle_spiral 900
let list_positions_prog_1 = run_rect spiral_program spiral_rect

(* gestion de la fenetre graphique*) 
let _ = 
  (* pour l'instant la hauteur et largeur de la fenetre sont parametrées manuellement, 
  mais après on récupère ça par les options*)
  open_graph " 900x900";
draw_axis 900 900;
print_rectangles list_positions_prog_1 900 900;
ignore (read_line ()); (*permet que la fenetre ne se ferme pas toute seule, elle attend que l'utilisatuer fasse entrée dans le terminal*)
close_graph ()

(* il reste à gérer un programme avec un either, pas sûre d'avoir compris ce qui était attendu pour ça
mais j'aurais la réponse par mail bientot
le reste des options est facile à gérer*)

(* reste à faire : 
rajouter 2 programmes
si on peut faire la spirale quand même c'est cool
je vais peut etre la faire sans les rotations et avec un carré ce sera dix fois plus simple
et comme ça on a quand même un programme stylé

implémenter les options, mais à voir leur réponse sur où elles sont passées par l'utilisateur
*)