
(* Code de la Section 3 du projet. *)

type coord2D = {
    x : float;
    y : float
  }
type point = coord2D
type vector = coord2D
type angle = float

let translate (v : vector) (p : point) : point =
  {x = p.x +. v.x; y = p.y +. v.y}

let rad_of_deg (a : angle) : angle =
  failwith "À compléter"

let deg_of_rad (a : angle) : angle =
  failwith "À compléter"

let rotate (c : point) (alpha : angle) (p : point) : point =
  {x = c.x +. (p.x -. c.x) *. Float.cos (Float.pi /. 180. *. alpha) -. (p.y -. c.y) *. Float.sin (Float.pi /. 180. *. alpha); 
   y = c.y +. (p.x -. c.x) *. Float.sin (Float.pi /. 180. *. alpha) +. (p.y -. c.y) *. Float.cos (Float.pi /. 180. *. alpha)} 
  
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
  r.x_min <= p.x && p.x <= r.x_max && r.y_min <= p.y && p.y <= r.y_max

let corners (r :rectangle) : point list =
  failwith "À compléter"
  
let rectangle_of_list (pl : point list) : rectangle = 
  failwith "À compléter"
