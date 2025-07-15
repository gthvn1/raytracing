let usage_msg =
  {|
Usage: main.exe [VIEWER] [PPM_FILENAME]

Starts the viewer on a PPM image file.

Arguments:
  VIEWER         The image viewer to use (default: xview)
  PPM_FILENAME   The PPM file to view (default: output.ppm)

Notes:
  - If only one argument is given, it is treated as the PPM_FILENAME.
  - If VIEWER is specified, PPM_FILENAME must also be provided.
|}

(* Default values *)
let viewer = ref "xview"
let ppm_file = ref "output.ppm"

let parse_args () =
  (* Parsing arguments if any *)
  let args = ref [] in
  Arg.parse [] (fun s -> args := !args @ [ s ]) usage_msg;
  match !args with
  | [ v; f ] ->
      viewer := v;
      ppm_file := f
  | [ f ] -> ppm_file := f
  | [] -> ()
  | _ ->
      prerr_endline usage_msg;
      exit 1

let () =
  parse_args ();

  (* Use 16:9 ratio helps to find transposition between x and y *)
  let aspect_ratio = 16.0 /. 9.0 in
  let image_width : int = 400 in

  (* Compute the image height and ensure that it is at least 1. *)
  let image_height = int_of_float (float_of_int image_width /. aspect_ratio) in
  let image_height : int = if image_height < 1 then 1 else image_height in

  (* Viewport widths less than one are oks since they are real valued. *)
  let actual_ratio = float_of_int image_width /. float_of_int image_height in
  let viewport_height : float = 2.0 in
  let viewport_width : float = viewport_height *. actual_ratio in

  (* Camera *)
  let focal_length = 1.0 in
  let camera_center = Vec3.make ~x:0.0 ~y:0.0 ~z:0.0 in

  (* Calculate the vectors across the horizontal and down the vertical viewport edges *)
  let viewport_u = Vec3.make ~x:viewport_width ~y:0.0 ~z:0.0 in
  let viewport_v = Vec3.make ~x:0.0 ~y:(-.viewport_height) ~z:0.0 in

  (* Calculate the horizontal and vertical delta vectors from pixel to pixel *)
  let pixel_delta_u = Vec3.(viewport_u // float_of_int image_width) in
  let pixel_delta_v = Vec3.(viewport_v // float_of_int image_height) in

  (* Calculate the location of the upper left pixel *)
  let focal = Vec3.make ~x:0.0 ~y:0.0 ~z:focal_length in
  let viewport_upper_left = Vec3.(camera_center --- focal) in
  let viewport_upper_left =
    Vec3.(viewport_upper_left --- (viewport_u // 2.0))
  in
  let viewport_upper_left =
    Vec3.(viewport_upper_left --- (viewport_v // 2.0))
  in
  let pixel00_loc = Vec3.(pixel_delta_u +++ pixel_delta_v) in
  let pixel00_loc = Vec3.(pixel00_loc ** 0.5) in
  let pixel00_loc : Vec3.t = Vec3.(viewport_upper_left +++ pixel00_loc) in

  ignore pixel00_loc;

  Printf.printf "Image width    : %d\n" image_width;
  Printf.printf "Image height   : %d\n" image_height;
  Printf.printf "Actual ratio   : %f\n" actual_ratio;
  Printf.printf "Viewport width : %f\n" viewport_width;
  Printf.printf "Viewport height: %f\n" viewport_height;
  flush stdout;

  let pixels =
    Ppm.render ~width:image_width ~height:image_height ~pixel00:pixel00_loc
      ~pixel_du:pixel_delta_u ~pixel_dv:pixel_delta_v ~camera_center
  in
  match Ppm.generate ~width:image_width ~height:image_height ~pixels with
  | Ok ppm_img ->
      let oc = open_out !ppm_file in
      output_string oc ppm_img;
      close_out oc;
      let cmd = Filename.quote_command !viewer [ !ppm_file ] in
      ignore (Sys.command cmd)
  | Error e ->
      prerr_endline ("Error: " ^ e);
      exit 1
