type t = float * float * float

(* Accessors *)
let x (x, _, _) = x
let y (_, y, _) = y
let z (_, _, z) = z

(* Length functions *)
let length_squared (x, y, z) = (x *. x) +. (y *. y) +. (z *. z)
let length v = sqrt (length_squared v)

(* Operations with scalar *)
let ( ++ ) (x, y, z) s = (x +. s, y +. s, z +. s)
let ( -- ) (x, y, z) s = (x -. s, y -. s, z -. s)
let ( ** ) (x, y, z) s = (x *. s, y *. s, z *. s)
let ( // ) (x, y, z) s = (x /. s, y /. s, z /. s)

(* Operations between vectors *)
let ( +++ ) (x1, y1, z1) (x2, y2, z2) = (x1 +. x2, y1 +. y2, z1 +. z2)
let ( --- ) (x1, y1, z1) (x2, y2, z2) = (x1 -. x2, y1 -. y2, z1 -. z2)
let ( *** ) (x1, y1, z1) (x2, y2, z2) = (x1 *. x2, y1 *. y2, z1 *. z2)
let ( /// ) (x1, y1, z1) (x2, y2, z2) = (x1 /. x2, y1 /. y2, z1 /. z2)
let dot (x1, y1, z1) (x2, y2, z2) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

let cross (x1, y1, z1) (x2, y2, z2) =
  ((y1 *. z2) -. (z1 *. y2), (z1 *. x2) -. (x1 *. z2), (x1 *. y2) -. (y1 *. x2))

let unit_vector v = v // length v
