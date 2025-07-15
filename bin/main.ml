let () =
  let width = 256 in
  let height = 256 in
  let pixels = Ppm.green_pixels (width * height) in
  match Ppm.create ~width ~height ~pixels with
  | Ok ppm_img ->
      let oc = open_out "output.ppm" in
      output_string oc ppm_img;
      close_out oc
  | Error e -> Printf.eprintf "Error: %s" e
