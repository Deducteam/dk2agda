(** Things to do sorted by importance :
    Won't work if not done :
    * resolve the data..where problem
    * resolve the module problem 
    * add support for everything in Sign.t ?
    Will work if not done :
    * add more explicit comments and types (cf comments in lambdapi)
    * beautify the code (right now it's a bit messy since I still have issues) 
*)
(** This version type-checks 30/125 test/OK files *)

open Core
open Core.Terms
open Core.Pos
open Core.Basics
open Core.Files
open Core.Extra

open Filename
open Timed

let forbidden_id = ref ["abstract";"consructor";"data";"do";"eta-equality"
                        ;"field";"forall";"hiding";"import";"in";"inductive"
                        ;"infix";"infixl";"infixr";"instance";"let";"macro"
                        ;"module";"mutual";"no-eta-equality";"open";"overlap"
                        ;"pattern";"postulate";"primitive";"private";"public"
                        ;"quote";"quoteContext";"quoteGoal";"quoteTerm";"record"
                        ;"renaming";"rewrite";"Set";"syntax";"tactic";"unquote"
                        ;"unquoteDecl";"unquoteDef";"using";"variable";"where"
                        ;"with";"Prop";"Set"]

let sanitize id =
  if id = "_" 
  then id
  else
    if Str.string_match (Str.regexp "[0-9]+$") id 0 || List.mem id !forbidden_id
    then 
      "dk^"^id
    else
      let regexp = Str.regexp "_" in
      Str.global_replace regexp "::" id

let rec list_to_str : string -> string list -> string = fun sep -> fun l ->
  match l with 
  | []    -> ""
  | [x]   -> x
  | x::xs -> x ^ sep ^ list_to_str sep xs

let sym_get_name = fun s -> sanitize s.sym_name
let ps = Printf.sprintf

(** actual printing functions *)

(** make a string out of term and returns it *)
let rec term_to_str : term -> string = fun term ->
  let san_name_of = fun x -> sanitize (Bindlib.name_of x) in
  match unfold term with
  | Vari(var) -> san_name_of var 
  | Type -> "Set"
  | Kind -> "Set1"
  | Symb(s) -> sym_get_name s
  | Prod(a,b) -> 
      let (x,b) = Bindlib.unbind b in 
      ps "(%s : %s) -> %s" (san_name_of x) (term_to_str a) (term_to_str b)
  | Abst(a,t) -> 
      let (x,t) = Bindlib.unbind t in 
      ps "\\(%s : %s) -> %s" (san_name_of x) (term_to_str a) (term_to_str t)
  | Appl(l,r) -> Printf.sprintf "(%s (%s))" (term_to_str l) (term_to_str r)
  | Wild -> "_" 
  | LLet(_,t,u) -> term_to_str (Bindlib.subst u t)
  | Patt(_,n,_) ->
          (* /!\ This is a hack to remove the leading v *)
          let w_v = (sanitize n) in
          let wo_v = String.sub w_v 1 ((String.length w_v) - 1) in
          ps "%s" (if w_v.[0] = 'v' then wo_v else w_v)
  (** removed by unfold *)
  | Meta(_,_)
  | TEnv(_,_)
  | TRef(_) -> "" 

let lhs_to_str : term list -> string = fun lhs ->
  list_to_str " " (List.map term_to_str lhs)

(** /!\ Problem with "data %s where\n" *)
(** I think that what's actually happening is that constructors for data are not
    writen right atfter. So I need to bring them all up, right after the 
    data..where clause in the symbol Map (well in the List done afterwards) *)
let symbol_type_to_str : sym -> string = fun s -> 
  match (!(s.sym_def),!(s.sym_rules)) with
  | (None,[]) -> (* no def or rule *)
    ps "postulate %s : %s\n" (sym_get_name s) (term_to_str !(s.sym_type))
  | (Some(_),_)
  | (None,_) -> 
      ps "%s : %s\n" (sym_get_name s) (term_to_str !(s.sym_type))

let symbol_defi_to_str : sym -> string = fun s ->
  match !(s.sym_def) with
  | Some(d) -> ps "%s = %s\n" (sym_get_name s) (term_to_str d) 
  | None -> ""

let symbol_rule_to_str : sym -> string = fun s ->
  let fn = fun acc e ->
    let name = sym_get_name s in
    let lhs = lhs_to_str e.lhs in
    let rhs = term_to_str (term_of_rhs e) in
    acc ^ (ps "%s %s = %s\n" name lhs rhs) in
  List.fold_left fn "" !(s.sym_rules) 

(** Print symbols sorted by their line_number *)
(** /!\ I still need to make symbols representing data (such as Nat) to use 'data .. where\n' syntax *)
(** I separated definition, rule, and type printing to be able to expreiment *)
let print_symbols : out_channel -> (sym * popt) StrMap.t ref -> unit = 
  fun oc -> fun map ->
  let get_pos = fun opt_pos ->
    match opt_pos with
    | Some(p) -> (Lazy.force p).start_line
    | None -> 0
  in
  let sorted_symbols =
    let l = StrMap.fold (fun _ (s,p) acc -> (s,p) :: acc) !map [] in
    List.sort (fun e1 e2 -> compare (get_pos (snd e1)) (get_pos (snd e2))) l
  in
  List.iter (fun (sym,_) -> 
    let stype = symbol_type_to_str sym in
    let sdefi = symbol_defi_to_str sym in
    let srule = symbol_rule_to_str sym in
    Printf.fprintf oc "%s%s%s" stype sdefi srule) sorted_symbols

(** prints "open import" directives for dependencies *)
(** /!\ Dependency path are relative to root of the library *)
let print_deps : out_channel -> (string * rule) list PathMap.t ref -> unit = 
  fun oc -> fun map ->
  PathMap.iter (fun path _ -> 
    let str = (sanitize (list_to_str "." path)) in 
    if str.[0] != '.' then Printf.fprintf oc "open import %s\n" str) !map

(** prints "module ... where" directives for the current file *)
let print_module : out_channel -> Path.t -> unit = fun oc -> fun path ->
  Printf.fprintf oc "module %s where\n" (sanitize (List.hd (List.rev path)))

(** main export function, print differents parts of the file to oc *)
let export : out_channel -> Sign.t -> unit = fun oc -> fun s ->
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
    let filename = sanitize (remove_extension (basename Sys.argv.(1))) in
    let oc = open_out (Sys.argv.(2) ^ dir_sep ^ filename ^ ".agda") in
    let _ = Sys.chdir (dirname Sys.argv.(1)) in
    let _ = init_lib_root () in
    let sign = Compile.compile_file (basename Sys.argv.(1)) in
    export oc sign 
