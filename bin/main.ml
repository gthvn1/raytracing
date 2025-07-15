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

  let width = 256 in
  let height = 256 in
  let pixels = Ppm.hello_world ~width ~height in

  match Ppm.generate ~width ~height ~pixels with
  | Ok ppm_img ->
      let oc = open_out !ppm_file in
      output_string oc ppm_img;
      close_out oc;
      let cmd = Filename.quote_command !viewer [ !ppm_file ] in
      ignore (Sys.command cmd)
  | Error e ->
      prerr_endline ("Error: " ^ e);
      exit 1
