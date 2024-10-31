
(* Code de la Section 3 du projet. *)

type coord2D = {
    x : float;
    y : float
  }
type point = coord2D
type vector = coord2D
type angle = float

let translate (v : vector) (p : point) : point =
  {x = p.x +. v.x  ; y = p.y +. v.y };;

let rad_of_deg (a : angle) : angle =
  Float.pi *. a /. 180.0 ;;

let deg_of_rad (a : angle) : angle =
  a *. 180.0 /. Float.pi ;;

let rotate (c : point) (alpha : angle) (p : point) : point =
(* on veut translater p par -c pour appliquer une rotation par rapport à l'origine*)
let neg_c = { x = (-1.) *. c.x ; y = (-1.) *. c.y} in

(* on code une fonction de rotation par rapport à l'origine*)
let rotation_vectorielle (theta : angle) (m : point) =
   {x = (Float.cos theta) *. m.x -. (Float.sin theta) *. m.y ; y = (Float.sin theta) *. m.x +. (Float.cos theta) *. m.y}

   (*on translate p par -c, puis on applique la rotation par rapport à l'origine, puis on retranslate de c*)
in translate c (rotation_vectorielle (rad_of_deg alpha) (translate neg_c p));;
  
type transformation =
  Translate of vector
| Rotate of point * angle

let transform (t : transformation) (p : point) : point =
  match t with 
  | Translate v -> translate v p
  | Rotate (c, alpha) -> rotate c alpha p 

type rectangle = {
    x_min : float;
    x_max : float;
    y_min : float;
    y_max : float
  }

let in_rectangle (r : rectangle) (p : point) : bool =
  (r.x_min <= p.x) && (r.x_max >= p.x) && (r.y_min <= p.y) && (r.y_max >= p.y)

let corners (r : rectangle) : point list =
  [{x = r.x_min ; y = r.y_max} ; {x = r.x_min ; y = r.y_min} ; {x = r.x_max ; y = r.y_max} ; {x = r.x_max ; y = r.y_min}]
  
let rectangle_of_list (pl : point list) : rectangle = 
  failwith "À compléter"
