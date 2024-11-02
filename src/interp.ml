open Geo

(* Code de la Section 4 du projet. *)

type instruction =
  Move of transformation
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

let unfold_repeat (prog : program) : program =
  failwith "À compléter"

let run_det (prog : program) (p : point) : point list =
  failwith "À compléter"

let target_reached_det (prog : program) (p : point) (target : rectangle) : bool =
  failwith "À compléter"
  
let run (prog : program) (p : point) : point list =
  failwith "À compléter"

let all_choices (prog : program) : program list =
  failwith "À compléter"

let target_reached (prog : program) (p : point) (r : rectangle) : bool =
  failwith "À compléter"
  
