(** Things to do sorted by importance :
    Won't work if not done :
    * resolve the lhs problem :
      - lhs_solve in lhs_to_str doesn't work so this only works on a modified
        version of lambdapi
    Will work if not done :
    * add support for everything in Sign.t ? (at least op ?)
    * recreate infile dir structure (as of today, it is flattenned)
    * add more explicit comments and types (cf comments in lambdapi)
*)
(** This version type-checks 76/125 test/OK files (60.8%) *)

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

(** [sanitize s] sanitizes [s] so that it matches Agda's syntax *)
let sanitize : string -> string = fun id ->
  if id = "_" 
  then id
  else 
    (* Matches numbers or Agda-keywords since Agda can't have them as idents *)
    if Str.string_match (Str.regexp "[0-9]+$") id 0 || List.mem id !forbidden_id
    then 
      "dk^"^id
    else
      let regex1 = Str.regexp "_" in
      let regex2 = Str.regexp "\\$\\|{|\\||}" in
      Str.global_replace regex2 "" (Str.global_replace regex1 "^" id) 

(** [list_to_str s l] makes a string from [l] putting [s] between each elements
    of [l] *)
let rec list_to_str : string -> string list -> string = fun sep -> fun l ->
  match l with 
  | []    -> ""
  | [x]   -> x
  | x::xs -> x ^ sep ^ list_to_str sep xs

(** [sym_get_name s] returns the sanitized name of symbol [s] *)
let sym_get_name : sym -> string = fun s -> sanitize s.sym_name

(** [ps] handy alias for Printf.sprintf *)
let ps = Printf.sprintf

(** [san_name_of e] returns the sanitized name of [e] using Bindlib.name_of *)
let san_name_of = fun x -> sanitize (Bindlib.name_of x)

(** {3 Printing functions} *)

(** [term_to_str t] makes a string out of [t] following Agda's syntax *)
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
        ps "(%s -> %s)" (term_to_str a) (term_to_str b)
      else
        ps "((%s : %s) -> %s)" name (term_to_str a) (term_to_str b)
  | Abst(a,t) -> 
      let (x,t) = Bindlib.unbind t in 
      ps "(\\(%s : %s) -> %s)" (san_name_of x) (term_to_str a) (term_to_str t)
  | Appl(l,r) -> Printf.sprintf "((%s) %s)" (term_to_str l) (term_to_str r)
  | Wild -> "_" 
  | LLet(_,t,u) -> term_to_str (Bindlib.subst u t)
  | Patt(None,_,_) -> "_"
  | Patt(Some(i),n,_) -> sanitize n
  | _ -> "" 

(* WIP : replaces Patt by fresh metas.
   Typing.infer_constr calls Sign.current_sign.() at some point and fails.

let solve_lhs : term list -> term list = fun lhs ->
  let rec lhs_subst = fun e ->
    match e with
    (* here, type, arity and Some(i)/None isn't used (but probably should) *)
    | Patt(_,name,_) -> Meta(fresh_meta ~name Type 0,[||])
    (* rec calls *)
    | Prod(a,b) ->
        Prod (lhs_subst a, lhs_subst_binder b)
    | Abst(a,b) -> 
        Abst (lhs_subst a, lhs_subst_binder b)
    | Appl(l,r) -> Appl(lhs_subst l, lhs_subst r)
    | LLet(a,t,u) -> LLet(lhs_subst a, lhs_subst t, lhs_subst_binder u)
    (* leafs *)
    | _ -> e
  and lhs_subst_binder = fun b -> 
    let (x,b) = Bindlib.unbind b in
    Bindlib.unbox (Bindlib.bind_var x (lift (lhs_subst b)))
  in
  let _ = List.map (fun e -> Typing.infer_constr [] (lhs_subst e)) lhs in
  lhs (* effets de bord dans infer_constr *) (* appel a  *)
*)

(** [lhs_to_str lhs] transforms [lhs] to string *)
let lhs_to_str : term -> string = fun lhs ->
  (*let nlhs = solve_lhs lhs in*)
  (*list_to_str " " (List.map term_to_str lhs)*)
  term_to_str lhs
(** {b NOTE} that this function has type term -> string but whould have 
    term list -> string when using rule.lhs . Here it's using the 
    lhs_typing obtained from sr.ml *)

(** [lhs_vars_to_str t] makes a string out of all the vars in [t]
    this is needed to use rewriting in Agda. For example, if there is 3 vars
    x y z in [t], this returns "∀ x -> ∀ y -> ∀ z -> "*)
let lhs_vars_to_str : string -> string = fun lhs_typing ->
  (* when using lhs_typing, vars isn't updated to remove 
     vars that were replaced by a new value so one needs
     to extract variables. *)
  let rec get_vars = fun acc pos lhs_str ->
    let regex = Str.regexp "\\(v[0-9]+[\\^]?[a-zA-Z]*\\)" in
    let t = (try Str.search_forward regex lhs_str pos with Not_found -> (-1)) in
    if t = (-1) || t >= String.length lhs_str then acc
    else get_vars (Str.matched_string lhs_str::acc) (t + 1) lhs_str
  in
  (* Using only matching doesn't work because in lhs_typing, Vari and Patt
     are still Symb *)
  (*let rec get_vars = fun acc lhs_t ->
    match unfold lhs_t with
    | Patt(Some(i),n,_) -> (sanitize n)::acc
    | Vari(var) -> (san_name_of var)::acc
    | Prod(a,b)
    | Abst(a,b) -> 
        let (x,b) = Bindlib.unbind b in
        let left = get_vars ((san_name_of x)::acc) a in
        get_vars left b
    | Appl(l,r) -> 
        let left = get_vars acc l in
        get_vars left r
    | LLet(_,t,u) -> get_vars acc (Bindlib.subst u t)
    | Symb -> acc
    | _ -> acc
  in*)
  let l = get_vars [] 0 lhs_typing in
  (** non linear rules can have duplicates vars, so we need to remove them *)
  let wo_dup = 
    List.fold_left (fun acc e -> if List.mem e acc then acc else e::acc) [] l
  in
  List.fold_left (fun acc e -> acc ^ "∀ " ^ e ^ " -> ") "" wo_dup

(** [symbol_type_to_str s] makes a string out of [s] type  
    following Agda's syntax *)
let symbol_type_to_str : sym -> string = fun s -> 
  ps "%s : %s\n" (sym_get_name s) (term_to_str !(s.sym_type)) 

(** [symbol_defi_to_str s] makes a string out of [s] definition 
    following Agda's syntax *)
let symbol_defi_to_str : sym -> string = fun s ->
  match !(s.sym_def) with
  | Some(d) -> ps "%s = %s\n" (sym_get_name s) (term_to_str d) 
  | None -> ""

(** [symbol_rule_to_str s] makes a string out of [s] rules
    following Agda's syntax *)
let symbol_rule_to_str : sym -> string = fun s ->
  let fn = fun acc e ->
    let name = sym_get_name s in
    let lhs = lhs_to_str e.lhs_typing in
    (*let lhs = lhs_to_str e.lhs in (* when solve_lhs will work *)*)
    let rhs = term_to_str (term_of_rhs e) in
    let lhs_vars = lhs_vars_to_str lhs in
    let rule_name = ps "rule^%s^%d" name !uid in
    let _ = incr uid in 
    let l1 =  (ps "%s : %s %s ≡ (%s)\n" rule_name lhs_vars lhs rhs) in
    acc ^ l1 ^ (ps "{-# REWRITE %s #-}\n" rule_name) 
  in
  List.fold_left fn "" !(s.sym_rules) 

(** Print symbols sorted by their line_number *)
(** [print_symbols oc map] prints symbols in [map] sorted by their
    line number to [oc]. Prints all types and definitions first,
    then rules. *)
let print_symbols : out_channel -> (sym * popt) StrMap.t ref -> unit = 
  fun oc -> fun map ->
  let get_pos = fun opt_pos ->
    match opt_pos with
    | Some(p) -> (Lazy.force p).start_line
    | None -> 0
  in
  (* sorting symbols by their line number *)
  let sorted_symbols =
    let l = StrMap.fold (fun _ (s,p) acc -> (s,p) :: acc) !map [] in
    List.sort (fun e1 e2 -> compare (get_pos (snd e1)) (get_pos (snd e2))) l
  in
  (* printing type and definition *)
  List.iter (fun (sym,_) ->
    let stype = symbol_type_to_str sym in
    let sdefi = symbol_defi_to_str sym in
    Printf.fprintf oc "%s%s" stype sdefi) sorted_symbols;
  (* printing rules *)
  List.iter (fun (sym,_) -> 
    let srule = symbol_rule_to_str sym in
    Printf.fprintf oc "%s" srule) sorted_symbols

(** [print_deps oc map] prints "open import ..." directives for all
    dependencies in [map] to [oc] except the .unif_rule dependecy. *)
let print_deps : out_channel -> (string * rule) list PathMap.t ref -> unit = 
  fun oc -> fun map ->
  (* removes unif_rule *)
  let nmap = PathMap.filter (fun path _ ->
    String.length (List.hd path) != 0) !map
  in
  (* needed for rewrite rules *)
  Printf.fprintf oc "open import Agda.Builtin.Equality\n";
  Printf.fprintf oc "open import Agda.Builtin.Equality.Rewrite\n";
  (* prints all other deps *)
  PathMap.iter (fun path _ -> 
    let str = (sanitize (List.hd (List.rev path))) in 
    Printf.fprintf oc "open import %s\n" str) nmap

(** [print_header oc p] prints "module ... where" (using [p]), and OPTIONS 
    pragma, to [oc] *)
let print_header : out_channel -> Path.t -> unit = fun oc -> fun path ->
  Printf.fprintf oc "{-# OPTIONS -W noMissingDefinitions --rewriting #-}\n";
  Printf.fprintf oc "module %s where\n" (sanitize (List.hd (List.rev path)))

(** [export oc s] prints dependencies, module, and symbols of [s] to [oc]*)
let export : out_channel -> Sign.t -> unit = fun oc -> fun s ->
  print_header oc s.sign_path;
  print_deps oc s.sign_deps;
  print_symbols oc s.sign_symbols

(* Entry point *)
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
