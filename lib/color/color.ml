(* Let's reuse the vec3 for colors *)
type t = Vec3.t

(* Accessors *)
let red (r, _, _) = r
let green (_, g, _) = g
let blue (_, _, b) = b

let make ~(r : float) ~(g : float) ~(b : float) : t =
  (* Ensure that parameters are in the correct range *)
  let check name value =
    if value < 0.0 || value > 1.0 then
      invalid_arg
        (Printf.sprintf
           "Color.make: %s component %.2f is out of range [0.0, 1.0]" name value)
  in
  check "r" r;
  check "g" g;
  check "b" b;
  (r, g, b)

let to_string (c : t) : string =
  let open Vec3 in
  (* Translate the [0, 1] component values to the byte range [0, 255]. *)
  let tr = c ** 255.999 in
  let r = string_of_int (int_of_float (red tr)) in
  let g = string_of_int (int_of_float (green tr)) in
  let b = string_of_int (int_of_float (blue tr)) in
  r ^ " " ^ g ^ " " ^ b ^ "\n"
