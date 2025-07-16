(* Origin is a point but let's consider is a Vec3.t for now *)
type t = { orig : Vec3.t; dir : Vec3.t }

let make ~origin ~direction = { orig = origin; dir = direction }

(* accessors *)
let origin (r : t) = r.orig
let direction (r : t) = r.dir

let at (r : t) (time : float) : Vec3.t =
  let open Vec3 in
  r.orig +++ (r.dir ** time)

let hit_sphere (center : Vec3.t) (radius : float) (ray : t) : bool =
  let open Vec3 in
  let oc = center --- ray.orig in
  let a = dot ray.dir ray.dir in
  let b = -2.0 *. dot ray.dir oc in
  let c = dot oc oc -. (radius *. radius) in
  let discriminant = (b *. b) -. (4.0 *. a *. c) in
  discriminant >= 0.0

let color (r : t) : Color.t =
  let open Vec3 in
  if hit_sphere (0.0, 0.0, -1.0) 0.5 r then Color.make ~r:1.0 ~g:0.0 ~b:0.0
  else
    let unit_direction = r |> direction |> unit_vector in
    let a = (y unit_direction +. 1.0) *. 0.5 in
    let start_color = Color.make ~r:1.0 ~g:1.0 ~b:1.0 in
    let end_color = Color.make ~r:0.5 ~g:0.7 ~b:1.0 in
    (start_color ** (1.0 -. a)) +++ (end_color ** a)
