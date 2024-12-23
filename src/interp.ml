open Geo

(* Code de la Section 4 du projet. *)

type instruction =
|  Move of transformation
| Repeat of int * program
| Either of program * program 
and program = instruction list

(* Fonction qui vérifie si le programme en paramètre est déterministe, 
   i.e. le programme contient au moins une instruction Either *)

let rec is_deterministic (prog : program) : bool =
  List.for_all has_either_instruction prog (* Applique à chaque élément de la liste prog la fonction has_neither_instruction *)
and has_either_instruction instr = 
  match instr with 
  | Move t -> true
  | Repeat (number, sub_prog) -> is_deterministic sub_prog (* Récursion pour vérifier si le sous-programme est déterminiset ou non *)
  | Either _ -> false


(* Définition de l'exception si l'on rencontre un either *)
exception EitherEncountered;;

(* Fonction qui déplie chaque instruction Repeat *)
let rec unfold_repeat (prog : program) : program =
  (* Fonction auxiliaire permettant de dérouler n fois le sous-programme à l'intérieur du Repeat *)
  let rec expand_repeat n sub_prog = 
    if n <= 0 then []
    else (unfold_repeat sub_prog) @ (expand_repeat (n-1) sub_prog)
  in
  match prog with 
  | [] -> []
  | Move t :: rest -> Move t :: unfold_repeat rest
  | Repeat (i,p) :: rest -> (expand_repeat i p) @ (unfold_repeat rest) (* On utilise la fonction auxiliaire *)
  (* | Either _ :: _ -> raise EitherEncountered (* Exception levée lorssque l'on rencontre un Either *) *)
    (* On gère maintenant le cas du Either en dépliant les possibles Repeat à l'intérieur du Either *)
  | Either (first_prog, second_prog) :: rest -> 
    Either (unfold_repeat first_prog, unfold_repeat second_prog) :: unfold_repeat rest
  

(* Fonction qui renvoie une liste de toutes les positions visitées par le robot durant l'exécution 
   du programme détermininiste en paramètre*)
let rec run_det (prog : program) (p : point) : point list =
  let unfolded_prog = unfold_repeat prog in (* On déplie le programme pour ne plus avoir de Repeat *)
  (* Fonction auxiliaire qui calcule chaque position atteinte par l'instruction courante *)
  let rec execute_program prog current_point visited_points =
    match prog with
    | [] -> List.rev visited_points
    | Either _ :: _ -> raise EitherEncountered (* Exception levée lorsque l'on rencontre un Either *)
    | Repeat _ :: _ -> failwith "error in unfold_repeat in run_det" (* Inutile mais doit être présent sinon problème à la compilation *)
    | Move t :: rest ->
        let new_point =
          match t with (* Calcul de la nouvelle position en fonction de si l'instruction est une translation ou une rotation *)
          | Translate vector -> translate current_point vector
          | Rotate (center, angle) -> rotate center angle current_point
        in
        execute_program rest new_point (new_point :: visited_points)
  in
  execute_program unfolded_prog p [p]

(* Fonction qui vérifie si le robot arrive dans la cible après l'exécution du programme *)
let target_reached_det (prog : program) (p : point) (target : rectangle) : bool =
  let list_of_points = run_det prog p in (* On récupère la liste de toutes les positions visitées *)
  let last_point = (* On récupère le dernier élément de cette liste *)
    match List.rev list_of_points with
    | [] -> p
    | x :: _ -> x
  in
in_rectangle target last_point (* On vérifie si ce point est dans le rectangle cible ou non *)
  
(* Fonction simulant une exécution possible d'un programme quelconque *)

let run (prog : program) (p : point) : point list =
  let unfolded_prog = unfold_repeat prog in (* On déplie le programme pour ne plus avoir de Repeat *)
  (* Fonction auxiliaire qui calcule chaque position atteinte par l'instruction courante *)
  let rec execute_program prog current_point visited_points =
    match prog with
    | [] -> List.rev visited_points
    | Either (first_prog, second_prog) :: rest -> 
      let random = Random.bool () in (* Initialisation d'un nombre pris aléatoirement entre 0 et 1 *)
      (* On choisit un des deux programmes du Either en fonction de la valeur de notre random *)
      let chosen_prog = if random then first_prog else second_prog in
      execute_program (chosen_prog @ rest) current_point visited_points
    | Repeat _ :: _ -> failwith "error in unfold_repeat in run" (* Toujours inutile mais nécessaire pour la compilation *)
    | Move t :: rest ->
        let new_point =
          match t with (* Calcul de la nouvelle position en fonction de si l'instruction est une translation ou une rotation *)
          | Translate vector -> translate current_point vector
          | Rotate (center, angle) -> rotate center angle current_point
        in
        execute_program rest new_point (new_point :: visited_points)
  in
  execute_program unfolded_prog p [p]

(* Fonction qui renvoie la liste de tous les programmes possibles sans Either *)
let all_choices (prog : program) : program list =
  let unfolded_prog = unfold_repeat prog in (* On déplie le programme pour ne plus avoir de Repeat *)
  (* Fonction auxiliaire avec accumulateur *)
  let rec explore_choices prog acc =
    match prog with
    | [] -> [List.rev acc] (* On retourne une liste contenant le programme inversé *)
    | Move t :: rest -> explore_choices rest (Move t :: acc)
    | Repeat _ :: _ -> failwith "error in unfold_repeat" (* Toujours inutile mais nécessaire pour la compilation *)
    | Either (left, right) :: rest ->
        (* Exploration des deux branches de Either *)
        let left_choices = explore_choices (left @ rest) acc in
        let right_choices = explore_choices (right @ rest) acc in
        List.rev_append left_choices right_choices
  in
  explore_choices unfolded_prog []


(* Fonction qui vérifie si chaque exécution possible du programme atteint la cible *)
let rec target_reached (prog : program) (p : point) (r : rectangle) : bool =
  let all_prog = all_choices prog in (* On transforme notre programme en une liste de toutes les possibilités *)
  (* On vérifie pour chaque programme si le robot atteint la cible *)
  let rec aux list_of_prog point rect =
    match list_of_prog with
    | [] -> true 
    | prog :: rest -> target_reached_det prog p r && aux rest p r
  in
  aux all_prog p r


