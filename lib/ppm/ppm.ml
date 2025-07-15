(* PPM specification: https://netpbm.sourceforge.net/doc/ppm.html *)

(* The hello world of Graphics *)
let render ~(width : int) ~(height : int) ~(pixel00 : Vec3.t)
    ~(pixel_du : Vec3.t) ~(pixel_dv : Vec3.t) ~(camera_center : Vec3.t) :
    Color.t list =
  let res : Color.t list =
    List.init height (fun j ->
        Printf.eprintf "\rScanlines remaining: %d" (height - j);
        flush stderr;
        List.init width (fun i ->
            let open Vec3 in
            let pixel_ui = pixel_du ** float_of_int i in
            let pixel_vj = pixel_dv ** float_of_int j in
            let pixel_center = pixel_ui +++ pixel_vj in
            let pixel_center = pixel_center +++ pixel00 in
            let ray_direction = pixel_center +++ camera_center in
            Ray.(make ~origin:camera_center ~direction:ray_direction |> color)))
    |> List.flatten
  in
  Printf.eprintf "\rDone                            \n";
  flush stderr;
  res

let generate ~(width : int) ~(height : int) ~(pixels : Color.t list) :
    (string, string) result =
  (* Ensure that we have enough pixels *)
  if width * height <> List.length pixels then
    Error
      (Printf.sprintf "Expected %d pixels got %d" (width * height)
         (List.length pixels))
  else
    let hdr = Printf.sprintf "P3\n%d %d\n255\n" width height in
    (* At most, to print rgb we will need 12 bytes (3 digits * 3 + 2 spaces + \n).
       So let's create our buffer with this value. *)
    let buffer = Buffer.create (width * height * 12) in
    List.iter (fun c -> Printf.bprintf buffer "%s" (Color.write_color c)) pixels;
    Ok (hdr ^ Buffer.contents buffer)
