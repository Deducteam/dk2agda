open Core
open Core.Terms
open Filename
open Timed

let rec list_to_string = fun sep -> fun l ->
  match l with 
  | []    -> ""
  | [x]   -> x
  | x::xs -> x ^ sep ^ list_to_string sep xs

let print_module = fun oc -> fun path ->
  Printf.fprintf oc "module %s where\n" (List.hd (List.rev path))

let print_deps = fun oc -> fun map ->
  Files.PathMap.iter (fun path _ -> Printf.fprintf oc "open import %s\n" (list_to_string "." path)) !map

let rec term_to_string = fun term ->
  match term with
  | Vari(var) -> Bindlib.name_of var 
  | Type -> "Set"
  | Kind -> "Set1"
  | Symb(s) -> s.sym_name 
  | Prod(a,b) -> 
      let (x,b) = Bindlib.unbind b in 
      Printf.sprintf "(%s : %s) -> %s" (Bindlib.name_of x) (term_to_string a) (term_to_string b)
      (** \Pi x:a, b *)
  | Abst(a,t) -> 
      let (x,t) = Bindlib.unbind t in 
      Printf.sprintf "\\(%s : %s) -> %s" (Bindlib.name_of x) (term_to_string a) (term_to_string t)
      (** \ (x : a) -> t*)
  | Appl(l,r) -> Printf.sprintf "(%s) (%s)" (term_to_string l) (term_to_string r)
  | Meta(_,_) -> ""
  | Patt(_,_,_) -> ""
  | TEnv(_,_) -> ""
  | Wild -> "_"
  | TRef(_) -> ""
  | LLet(_,_,_) -> ""


let symbol_to_string = fun symbol ->
  Printf.sprintf "%s : %s\n" symbol.sym_name (term_to_string !(symbol.sym_type))

let print_symbols = fun oc -> fun map ->
  Extra.StrMap.iter (fun _ (sym,_) -> Printf.fprintf oc "%s\n" (symbol_to_string sym)) !map

let export : out_channel -> Sign.t -> unit = fun oc -> fun s ->
  print_module oc s.sign_path;
  print_deps oc s.sign_deps;
  print_symbols oc s.sign_symbols

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
