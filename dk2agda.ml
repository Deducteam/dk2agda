(** Things to do sorted by importance :
    Won't work if not done :
    * resolve the lhs problem
    * add support for everything in Sign.t ? (at least op ?)
    Will work if not done :
    * recreate infile dir structure (as of today, it is flattenned)
    * add more explicit comments and types (cf comments in lambdapi)
    * beautify the code (right now it's a bit messy since I still have issues) 
*)
(** This version type-checks 64/125 test/OK files (50%) *)

open Core
open Core.Terms
open Core.Pos
open Core.Basics
open Core.Files
open Core.Extra

open Filename
open Timed

let uid = ref 0

(* refl added since it is imported in Agda.Builtin.Equality *)
let forbidden_id = ref ["abstract";"consructor";"data";"do";"eta-equality"
                        ;"field";"forall";"hiding";"import";"in";"inductive"
                        ;"infix";"infixl";"infixr";"instance";"let";"macro"
                        ;"module";"mutual";"no-eta-equality";"open";"overlap"
                        ;"pattern";"postulate";"primitive";"private";"public"
                        ;"quote";"quoteContext";"quoteGoal";"quoteTerm";"record"
                        ;"renaming";"rewrite";"Set";"syntax";"tactic";"unquote"
                        ;"unquoteDecl";"unquoteDef";"using";"variable";"where"
                        ;"with";"Prop";"Set";"refl"]

let sanitize id =
  if id = "_" 
  then id
  else 
    if Str.string_match (Str.regexp "[0-9]+$") id 0 || List.mem id !forbidden_id
    then 
      "dk^"^id
    else
      let regexp = Str.regexp "_" in
      Str.global_replace regexp "^" id

let rec list_to_str : string -> string list -> string = fun sep -> fun l ->
  match l with 
  | []    -> ""
  | [x]   -> x
  | x::xs -> x ^ sep ^ list_to_str sep xs

let sym_get_name = fun s -> sanitize s.sym_name
let ps = Printf.sprintf

let san_name_of = fun x -> sanitize (Bindlib.name_of x)

(** actual printing functions *)

(** make a string out of term and returns it *)
let rec term_to_str : term -> string = fun term ->
  match unfold term with
  | Vari(var) -> san_name_of var
  | Type -> "Set"
  | Kind -> "Set1"
  | Symb(s) -> sym_get_name s
  | Prod(a,b) -> 
      let (x,b) = Bindlib.unbind b in
      let name = san_name_of x in
      if name = "_"
      then
        ps "((%s) -> (%s))" (term_to_str a) (term_to_str b)
      else
        ps "((%s) : %s) -> %s" name (term_to_str a) (term_to_str b)
  | Abst(a,t) -> 
      let (x,t) = Bindlib.unbind t in 
      ps "\\(%s : %s) -> %s" (san_name_of x) (term_to_str a) (term_to_str t)
  | Appl(l,r) -> Printf.sprintf "(%s (%s))" (term_to_str l) (term_to_str r)
  | Wild -> "_" 
  | LLet(_,t,u) -> term_to_str (Bindlib.subst u t)
  | Patt(None,_,_) -> "_"
  | Patt(Some(i),n,_) -> ps "%s" (sanitize n) 
  (** removed by unfold *)
  | Meta(_,_)
  | TEnv(_,_)
  | TRef(_) -> "" 

let lhs_to_str : term list -> string = fun lhs ->
  list_to_str " " (List.map term_to_str lhs)

let symbol_type_to_str : sym -> string = fun s -> 
  ps "%s : %s\n" (sym_get_name s) (term_to_str !(s.sym_type)) 

let symbol_defi_to_str : sym -> string = fun s ->
  match !(s.sym_def) with
  | Some(d) -> ps "%s = %s\n" (sym_get_name s) (term_to_str d) 
  | None -> ""

let lhs_vars_to_str : rule -> string = fun r ->
  let fn = fun acc e ->
    let name = (san_name_of e) in
    acc ^ if name = "_" then "" else ("∀ " ^ name ^ " -> ")
  in
  Array.fold_left fn "" r.vars

(* using ≡ *)
(** /!\ need to replace lhs var unused in rhs with _ !!! *)
(** /!\ need to remove lhs var unused in rhs in lhs_vars 
    (these are the one written like "v[0-9]+" *)
(** Sol: make a san_rule : term -> term
 * that changes Patt(_,n,_) to Patt(_,"_",_) when n not in rule.vars *)
let symbol_rule_to_str : sym -> string = fun s ->
  let fn = fun acc e ->
    let name = sym_get_name s in
    let lhs = lhs_to_str e.lhs in
    let rhs = term_to_str (term_of_rhs e) in
    let rule_name = ps "rule^%s^%d" name !uid in
    let lhs_vars = lhs_vars_to_str e in
    let _ = incr uid in
    let l1 =  (ps "%s : %s %s %s ≡ (%s)\n" rule_name lhs_vars name lhs rhs) in
    acc ^ l1 ^ (ps "{-# REWRITE %s #-}\n" rule_name) 
  in
  List.fold_left fn "" !(s.sym_rules) 

(** Print symbols sorted by their line_number *)
(** I separated definition, rule, and type printing to be able to experiment *)
(** Order needs to be improved *)
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
    Printf.fprintf oc "%s%s" stype sdefi) sorted_symbols;
  List.iter (fun (sym,_) -> 
    let srule = symbol_rule_to_str sym in
    Printf.fprintf oc "%s" srule) sorted_symbols

(** prints "open import" directives for dependencies *)
let print_deps : out_channel -> (string * rule) list PathMap.t ref -> unit = 
  fun oc -> fun map ->
  (* remove unif_rule *)
  let nmap = PathMap.filter (fun path _ ->
    String.length (List.hd path) != 0) !map
  in
  (* print all other deps *)
  PathMap.iter (fun path _ -> 
    let str = (sanitize (List.hd (List.rev path))) in 
    Printf.fprintf oc "open import %s\n" str) nmap

(** prints "module ... where" directives for the current file *)
let print_module : out_channel -> Path.t -> unit = fun oc -> fun path ->
  Printf.fprintf oc "module %s where\n" (sanitize (List.hd (List.rev path)))

(** main export function, print differents parts of the file to oc *)
let export : out_channel -> Sign.t -> unit = fun oc -> fun s ->
  Printf.fprintf oc "{-# OPTIONS -W noMissingDefinitions --rewriting #-}\n";
  print_module oc s.sign_path;
  print_deps oc s.sign_deps;
  Printf.fprintf oc "open import Agda.Builtin.Equality\n";
  Printf.fprintf oc "open import Agda.Builtin.Equality.Rewrite\n";
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
