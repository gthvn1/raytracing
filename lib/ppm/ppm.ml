(* PPM specification: https://netpbm.sourceforge.net/doc/ppm.html *)
type render_params = {
  image_width : int;
  image_height : int;
  pixel00 : Vec3.t; (* the upper left pixel *)
  pixel_du : Vec3.t; (* horizontal delta vectors from pixel to pixel *)
  pixel_dv : Vec3.t; (* vertical delta vectors from pixel to pixel *)
  camera : Vec3.t; (* position of the camera *)
}

(* A blue-to-white gradient depending on ray Y coordinate *)
let render (p : render_params) : Color.t list =
  let res : Color.t list =
    List.init p.image_height (fun j ->
        Printf.eprintf "\rScanlines remaining: %d" (p.image_height - j);
        flush stderr;
        List.init p.image_width (fun i ->
            let open Vec3 in
            (* Get the pixel we are hitting:
                - deplacement relative to delta_u
                - deplacement relative to delta_v
                - get the center of the pixel by adding deplacement
                - the direction is the difference between pixel and the camera
             *)
            let pixel_ui = p.pixel_du ** float_of_int i in
            let pixel_vj = p.pixel_dv ** float_of_int j in
            let pixel_center = p.pixel00 +++ (pixel_ui +++ pixel_vj) in
            let ray_direction = pixel_center --- p.camera in
            Ray.(make ~origin:p.camera ~direction:ray_direction |> color)))
    |> List.flatten
  in
  Printf.eprintf "\rDone                            \n";
  flush stderr;
  res

let generate ~(width : int) ~(height : int) ~(pixels : Color.t list) : string =
  (* Ensure that we have enough pixels *)
  if width * height <> List.length pixels then
    invalid_arg
      (Printf.sprintf "Ppm.generate: expected %d pixels got %d" (width * height)
         (List.length pixels));

  let hdr = Printf.sprintf "P3\n%d %d\n255\n" width height in
  (* At most, to print rgb we will need 12 bytes (3 digits * 3 + 2 spaces + \n).
     So let's create our buffer with this value. *)
  let buffer = Buffer.create (width * height * 12) in
  List.iter (fun c -> Printf.bprintf buffer "%s" (Color.to_string c)) pixels;
  hdr ^ Buffer.contents buffer
