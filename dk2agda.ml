open Core
open Core.Terms
open Core.Pos
open Filename
open Timed

(** Some utility functions : *)
let rec list_to_string = fun sep -> fun l ->
  match l with 
  | []    -> ""
  | [x]   -> x
  | x::xs -> x ^ sep ^ list_to_string sep xs

(** actual printing functions *)

(** prints "module ... where" directives for the current file *)
(** /!\ doesn't match filename, either remove .lp/.dk from name or add it here *)
let print_module = fun oc -> fun path ->
  Printf.fprintf oc "module %s where\n" (List.hd (List.rev path))

(** prints "open import" directives for dependencies *)
(** /!\ The first dependencies is .unif_rule *)
let print_deps = fun oc -> fun map ->
  Files.PathMap.iter (fun path _ -> Printf.fprintf oc "open import %s\n" (list_to_string "." path)) !map

(** make a string out of term and returns it *)
(** /!\ not sure for LLet since I could use "let ... in ..." *)
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
      (** \(x : a) -> t*)
  | Appl(l,r) -> Printf.sprintf "(%s) (%s)" (term_to_string l) (term_to_string r)
      (** (l) (r) *)
  | Wild -> "_" 
  | LLet(_,t,u) -> term_to_string (Bindlib.subst u t)
  | Meta(_,_) 
      (** nope *)
  | Patt(_,_,_) 
      (** only in lhs *)
  | TEnv(_,_) 
      (** only in rhs *)
  | TRef(_) -> "" 
      (** only for surface matching *)

let lhs_to_string = fun lhs ->
  list_to_string " " (List.map (term_to_string) lhs)

(** returns a string from symbol *)
let symbol_to_string = fun symbol ->
  let stype = Printf.sprintf "%s : %s\n" symbol.sym_name (term_to_string !(symbol.sym_type)) in
  let sdef =
    match !(symbol.sym_def) with
    | Some(t) -> Printf.sprintf "%s = %s\n" symbol.sym_name (term_to_string t)
    | None -> ""
  in
  let srules = List.fold_left (fun acc e -> acc ^ (Printf.sprintf "%s %s = %s\n" symbol.sym_name (lhs_to_string e.lhs) ("rhs"))) "" !(symbol.sym_rules) in
  stype ^ sdef ^ srules

(** Print symbols sorted by their line_number *)
let print_symbols : out_channel -> (sym * popt) Extra.StrMap.t Timed.ref -> unit = fun oc -> fun map ->
  let get_pos = fun opt_pos ->
    match opt_pos with
    | Some(p) -> (Lazy.force p).start_line
    | None -> 0
  in
  let list = Extra.StrMap.fold (fun _ (s,p) acc -> (s,p) :: acc) !map [] in
  let syms = List.sort (fun e1 e2 -> compare (get_pos (snd e1)) (get_pos (snd e2))) list in
  List.iter (fun (sym,_) -> Printf.fprintf oc "%s\n" (symbol_to_string sym)) syms

(** main export function, print differents parts of the file to oc *)
let export : out_channel -> Sign.t -> unit = fun oc -> fun s ->
  print_module oc s.sign_path;
  print_deps oc s.sign_deps;
  print_symbols oc s.sign_symbols (** when printing symbols, be careful of Pos.popt (for scope) *)

(** Entry point : 
    takes 2 arguments : a file and a output directory *)
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
