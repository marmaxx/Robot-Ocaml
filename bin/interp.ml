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

(* Fonction pour éviter d'écrire int_of_Int à chaque fois *)
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
    ((width / 2) + i central_point.x) 
    ((height / 2) - i central_point.y) 
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

(*let choose_prog width height abs cr axis_colour background_colour circle_colour rect_colour prog =
  let (x_min , y_min, x_max, y_max) = !abs in
  let rect = rectangle x_min y_min x_max y_max in
  match prog with 
  | "1" ->  
    if !cr then 
      if !abs_specified then print_points_rects list_positions_prog_1 width height axis_colour background_colour rect_colour circle_colour

  | "2" -> 

  | "3" ->

  | _ -> failwith "prog must be 1, 2 or 3 ! "*)

let verbose = ref false
let output_file = ref ""

(* Références pour les options *)
let cr = ref false
let abs_specified = ref false
let abs = ref (0.0, 0.0, 0.0, 0.0)
let bc = ref (255, 229, 153)
let fc = ref (153, 0, 0)
let rc = ref (7, 55, 99)
let pc = ref (156, 167, 207)
let size = ref (900, 900)

(* Pour stocker l'argument obligatoire "prog" *)
let prog = ref ""

(* Fonction pour les options de couleur 
let set_color ref_color r g b =
  ref_color := (r, g, b)*)

(* Fonction pour les arguments anonymes *)
let anon_fun arg =
  if !prog = "" then
    prog := arg
  else
    failwith "Too many arguments provided. Only one 'prog' argument is allowed."

(* Message d'usage *)
let usage_msg = "Usage: dune exec -- interp [options] prog\n
                Options are:\n
                -abs X_MIN Y_MIN X_MAX Y_MAX -> affichage de rectangles et approximation initiale\n
                -cr -> affichage de points\n
                -bc r v b -> couleur de l’arrière plan\n
                -fc r v b -> couleur de l’avant plan\n
                -rc r v b -> couleur du rectangle\n
                -pc r v b -> couleur du point\n
                -size W H -> la dimension de la fenêtre en pixels\n"

let update_abs (x_min, y_min, x_max, y_max) =
  abs := (x_min, y_min, x_max, y_max);
  abs_specified := true

(* Liste des options *)
let speclist = [
  ("-cr", Arg.Set cr, "Enable -cr option");

  ("-abs", Arg.Tuple [
    Arg.Float (fun x_min -> let (_ , y_min, x_max, y_max) = !abs in update_abs (x_min, y_min, x_max, y_max));
    Arg.Float (fun y_min -> let (x_min , _, x_max, y_max) = !abs in update_abs (x_min, y_min, x_max, y_max));
    Arg.Float (fun x_max -> let (x_min , y_min, _, y_max) = !abs in update_abs (x_min, y_min, x_max, y_max));
    Arg.Float (fun y_max -> let (x_min , y_min, x_max, _) = !abs in update_abs (x_min, y_min, x_max, y_max));
  ], "Set abs values");

  ("-abs", Arg.Unit (fun () -> abs_specified := true), "Indicates if -abs option was specified");

  ("-bc", Arg.Tuple [
      Arg.Int (fun r -> let (_, g, b) = !bc in bc := (r, g, b));
      Arg.Int (fun g -> let (r, _, b) = !bc in bc := (r, g, b));
      Arg.Int (fun b -> let (r, g, _) = !bc in bc := (r, g, b));
    ], "Set background color r g b");

  ("-fc", Arg.Tuple [
      Arg.Int (fun r -> let (_, g, b) = !fc in fc := (r, g, b));
      Arg.Int (fun g -> let (r, _, b) = !fc in fc := (r, g, b));
      Arg.Int (fun b -> let (r, g, _) = !fc in fc := (r, g, b));
    ], "Set foreground color r g b");

  ("-rc", Arg.Tuple [
      Arg.Int (fun r -> let (_, g, b) = !rc in rc := (r, g, b));
      Arg.Int (fun g -> let (r, _, b) = !rc in rc := (r, g, b));
      Arg.Int (fun b -> let (r, g, _) = !rc in rc := (r, g, b));
    ], "Set rectangle color r g b");

  ("-pc", Arg.Tuple [
      Arg.Int (fun r -> let (_, g, b) = !pc in pc := (r, g, b));
      Arg.Int (fun g -> let (r, _, b) = !pc in pc := (r, g, b));
      Arg.Int (fun b -> let (r, g, _) = !pc in pc := (r, g, b));
    ], "Set point color r g b");

  ("-size", Arg.Tuple [
      Arg.Int (fun w -> let (_,h) = !size in size := (w,h));
      Arg.Int (fun h -> let (w,_) = !size in size := (w,h));
    ], "Set size w h");
]

let has_origin x_min y_min x_max y_max = 
  let rect = rectangle x_min y_min x_max y_max in 
  let point = point 0. 0. in 
  in_rectangle rect point

let _ = 
  (* Pour l'instant la hauteur et largeur de la fenêtre sont paramétrées manuellement, 
      mais après on récupère ça par les options *)
  
  (* Parsing des arguments *)
  Arg.parse speclist anon_fun usage_msg;

  (* Vérification que l'argument obligatoire "prog" a été fourni *)
  if !prog = "" then begin
    prerr_endline "Error: Missing required argument 'prog'.";
    Arg.usage speclist usage_msg;
    exit 1
  end;

  (* Affichage des paramètres après parsing *)
  Printf.printf "Verbose: %b\n" !verbose;
  Printf.printf "Output file: %s\n" !output_file;
  Printf.printf "Prog: %s\n" !prog;

  let (x_min , y_min, x_max, y_max) = !abs in

  Printf.printf "(0,0) dans le rectangle : %b\n" (has_origin x_min y_min x_max y_max);
  
  Printf.printf "abs ? %b\n" !abs_specified;

  let (br, bg, bb) = !bc in

  let (fr, fg, fb) = !fc in

  let (rr, rg, rb) = !rc in

  let (pr, pg, pb) = !pc in

  let (width, height) = !size in

  (* Ouverture de la fenêtre avec les dimensions choisies *)
  let size_str = Printf.sprintf " %dx%d" width height in
  open_graph size_str;

  let background_colour = rgb br bg bb in
  let axis_colour = rgb fr fg fb in
  let rect_colour = rgb rr rg rb in
  let circle_colour = rgb pr pg pb in

  fill_background width height background_colour;

  draw_axis width height axis_colour;
  
  (* il faut utiliser la focntion choose_prog ici *)

  print_points_rects list_positions_prog_2 width height axis_colour background_colour rect_colour circle_colour;
  print_rectangles list_positions_prog_2 width height axis_colour background_colour rect_colour;
  print_points list_positions_prog_1_point width height axis_colour background_colour circle_colour;
  print_rectangles list_positions_prog_1 width height axis_colour background_colour rect_colour;
  
  ignore (read_line ()); (* Permet que la fenêtre ne se ferme pas toute seule, elle attend que l'utilisateur appuie sur Entrée dans le terminal *)
  close_graph ()
  