(* PPM specification: https://netpbm.sourceforge.net/doc/ppm.html *)
type rgb = int * int * int

(* generates a list of red pixels of size [sz] *)
let red : rgb = (255, 0, 0)
let green : rgb = (0, 255, 0)
let red_pixels (sz : int) : rgb list = List.init sz (fun _ -> red)
let green_pixels (sz : int) : rgb list = List.init sz (fun _ -> green)

let create ~(width : int) ~(height : int) ~(pixels : rgb list) :
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
