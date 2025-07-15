(* PPM specification: https://netpbm.sourceforge.net/doc/ppm.html *)
type rgb = int * int * int

(* The hello world of Graphics *)
let hello_world ~(width : int) ~(height : int) : rgb list =
  let width_float = float_of_int width in
  let height_float = float_of_int height in
  List.init height (fun j ->
      List.init width (fun i ->
          let r = float_of_int i /. (width_float -. 1.0) in
          let g = float_of_int j /. (height_float -. 1.0) in
          let b = 0.0 in

          ( int_of_float (255.999 *. r),
            int_of_float (255.999 *. g),
            int_of_float (255.999 *. b) )))
  |> List.flatten

let generate ~(width : int) ~(height : int) ~(pixels : rgb list) :
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
    List.iter (fun (r, g, b) -> Printf.bprintf buffer "%d %d %d\n" r g b) pixels;
    Ok (hdr ^ Buffer.contents buffer)
