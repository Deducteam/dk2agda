open Core
open Filename
open Timed

let rec list_to_string = fun sep -> fun l ->
  match l with 
  | []    -> ""
  | [x]   -> x
  | x::xs -> x ^ sep ^ list_to_string sep xs

let print_deps = fun oc -> fun map ->
  Files.PathMap.iter (fun path _ -> Printf.fprintf oc "open import %s\n" (list_to_string "." path)) !map

(** this is a stub *)
let export : out_channel -> Sign.t -> unit = fun oc -> fun s ->
  print_deps oc s.sign_deps

(** takes 2 arguments : a file and a output directory *)
let _ = 
  let nbArg = Array.length Sys.argv - 1 in
  if nbArg != 2 
  then 
    failwith "Usage : dk2agda [infile_path] [outdir]"
  else
    let oc = open_out (Sys.argv.(2) ^ dir_sep ^ basename Sys.argv.(1) ^ ".agda") in
    let _ = Sys.chdir (dirname Sys.argv.(1)) in
    let _ = Files.init_lib_root () in
    let sign = Compile.compile_file (basename Sys.argv.(1)) in
    export oc sign 
