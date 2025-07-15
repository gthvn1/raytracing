type t = { orig : Vec3.t; dir : Vec3.t }

let make ~origin ~direction = { orig = origin; dir = direction }

(* accessors *)
let origin (r : t) = r.orig
let direction (r : t) = r.dir

let at (r : t) (time : float) : Vec3.t =
  let open Vec3 in
  r.orig +++ (r.dir ** time)

let color (r : t) : Color.t =
  let open Vec3 in
  let unit_direction = r |> direction |> unit_vector in
  let a = (y unit_direction +. 1.0) *. 0.5 in
  let c1 = Color.make ~r:1.0 ~g:1.0 ~b:1.0 in
  let c2 = Color.make ~r:0.5 ~g:0.7 ~b:1.0 in
  (c1 ** (1.0 -. a)) +++ (c2 ** a)
