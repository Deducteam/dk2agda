open Core
open Core.Terms
open Core.Pos
open Core.Basics
open Core.Files
open Core.Extra

open Filename
open Timed

let rec list_to_string : string -> string list -> string = fun sep -> fun l ->
  match l with 
  | []    -> ""
  | [x]   -> x
  | x::xs -> x ^ sep ^ list_to_string sep xs

(** actual printing functions *)

(** make a string out of term and returns it *)
(** /!\ not sure for LLet since I could use "let ... in ..." *)
let rec term_to_string : term -> string = fun term ->
  match unfold term with
  | Vari(var) -> Bindlib.name_of var 
  | Type -> "Set"
  | Kind -> "Set1"
  | Symb(s) -> s.sym_name 
  | Prod(a,b) -> 
      let (x,b) = Bindlib.unbind b in 
      Printf.sprintf "(%s : %s) -> %s" (Bindlib.name_of x) (term_to_string a) (term_to_string b)
  | Abst(a,t) -> 
      let (x,t) = Bindlib.unbind t in 
      Printf.sprintf "\\(%s : %s) -> %s" (Bindlib.name_of x) (term_to_string a) (term_to_string t)
  | Appl(l,r) -> Printf.sprintf "(%s) (%s)" (term_to_string l) (term_to_string r)
  | Wild -> "_" 
  | LLet(_,t,u) -> term_to_string (Bindlib.subst u t)
  | Patt(_,n,_) ->
          Printf.sprintf "%s" n
  (** removed by unfold *)
  | Meta(_,_)
  | TEnv(_,_)
  | TRef(_) -> "" 

let lhs_to_string : term list -> string = fun lhs ->
  list_to_string " " (List.map term_to_string lhs)

(** returns a string from symbol *)
let symbol_to_string : sym -> string = fun symbol ->
  let stype = Printf.sprintf "%s : %s\n" symbol.sym_name (term_to_string !(symbol.sym_type)) in
  let sdef =
    match !(symbol.sym_def) with
    | Some(t) -> Printf.sprintf "%s = %s\n" symbol.sym_name (term_to_string t)
    | None -> ""
  in
  let fn = fun acc e -> acc ^ (Printf.sprintf "%s %s = %s\n" symbol.sym_name (lhs_to_string e.lhs) (term_to_string (term_of_rhs e))) in
  let srules = List.fold_left fn "" !(symbol.sym_rules) in
  stype ^ sdef ^ srules

(** Print symbols sorted by their line_number *)
let print_symbols : out_channel -> (sym * popt) StrMap.t ref -> unit = fun oc -> fun map ->
  let get_pos = fun opt_pos ->
    match opt_pos with
    | Some(p) -> (Lazy.force p).start_line
    | None -> 0
  in
  let list = StrMap.fold (fun _ (s,p) acc -> (s,p) :: acc) !map [] in
  let syms = List.sort (fun e1 e2 -> compare (get_pos (snd e1)) (get_pos (snd e2))) list in
  List.iter (fun (sym,_) -> Printf.fprintf oc "%s\n" (symbol_to_string sym)) syms

(** prints "open import" directives for dependencies *)
(** /!\ The first dependencies is .unif_rule *)
let print_deps : out_channel -> (string * rule) list PathMap.t ref -> unit = fun oc -> fun map ->
  PathMap.iter (fun path _ -> Printf.fprintf oc "open import %s\n" (list_to_string "." path)) !map

(** prints "module ... where" directives for the current file *)
let print_module : out_channel -> Path.t -> unit = fun oc -> fun path ->
  Printf.fprintf oc "module %s where\n" (List.hd (List.rev path))

(** main export function, print differents parts of the file to oc *)
let export : out_channel -> Sign.t -> unit = fun oc -> fun s ->
  Printf.fprintf oc "{-# OPTIONS -W noMissingDefinitions #-}\n";
  print_module oc s.sign_path;
  print_deps oc s.sign_deps;
  print_symbols oc s.sign_symbols

(** Entry point : 
    takes 2 arguments : a file and a output directory *)
let _ = 
  let nbArg = Array.length Sys.argv - 1 in
  if nbArg != 2 
  then 
    failwith "Usage : dk2agda [infile_path] [outdir]"
  else
    let oc = open_out (Sys.argv.(2) ^ dir_sep ^ (remove_extension (basename Sys.argv.(1))) ^ ".agda") in
    let _ = Sys.chdir (dirname Sys.argv.(1)) in
    let _ = init_lib_root () in
    let sign = Compile.compile_file (basename Sys.argv.(1)) in
    export oc sign 
