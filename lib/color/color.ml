(* Let's reuse the vec3 for colors *)
type t = Vec3.t

let make ~(r : float) ~(g : float) ~(b : float) : t = (r, g, b)

let write_color (c : t) : string =
  let open Vec3 in
  (* Translate the [0, 1] component values to the byte range [0, 255]. *)
  let tr = c ** 255.999 in
  let r = string_of_int (int_of_float (x tr)) in
  let g = string_of_int (int_of_float (y tr)) in
  let b = string_of_int (int_of_float (z tr)) in
  r ^ " " ^ g ^ " " ^ b ^ "\n"
