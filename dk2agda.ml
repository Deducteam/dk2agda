open Core
open Filename

(** this is a stub *)
let export : out_channel -> Sign.t -> unit = fun oc -> fun s -> 
  Printf.fprintf oc "path is %s" (List.hd s.sign_path)

(** takes 2 arguments : a file and a output directory *)
let () = 
  let nbArg = Array.length Sys.argv - 1 in
  if nbArg != 2 
  then 
    failwith "Usage : dk2agda [infile_path] [outdir]"
  else
    let oc = open_out (Sys.argv.(2) ^ dir_sep ^ basename Sys.argv.(1) ^ ".agda") in
    let _ = Sys.chdir (dirname Sys.argv.(1)) in
    let sign = Compile.compile_file (basename Sys.argv.(1)) in
    export oc sign 
