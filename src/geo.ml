
(* Code de la Section 3 du projet. *)

type coord2D = {
    x : float;
    y : float
  }
type point = coord2D
type vector = coord2D
type angle = float

let translate (v : vector) (p : point) : point =
  failwith "À compléter"

let rad_of_deg (a : angle) : angle =
  failwith "À compléter"

let deg_of_rad (a : angle) : angle =
  failwith "À compléter"

let rotate (c : point) (alpha : angle) (p : point) : point =
  failwith "À compléter"
  
type transformation =
  Translate of vector
| Rotate of point * angle

let transform (t : transformation) (p : point) : point =
  failwith "À compléter"

type rectangle = {
    x_min : float;
    x_max : float;
    y_min : float;
    y_max : float
  }

let in_rectangle (r : rectangle) (p : point) : bool =
  failwith "À compléter"

let corners (r :rectangle) : point list =
  failwith "À compléter"
  
let rectangle_of_list (pl : point list) : rectangle = 
  failwith "À compléter"
