(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, * CNRS-Ecole Polytechnique-INRIA Futurs-Universite Paris Sud *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*i $Id: scheme.ml 11559 2008-11-07 22:03:34Z letouzey $ i*)

(*s Production of Scheme syntax. *)

open Pp
open Util
open Names
open Nameops
open Libnames
open Miniml
open Mlutil
open Table
open Common

let (@@) f g = f g

let keywords =
  List.fold_right (fun s -> Idset.add (id_of_string s))
    ["alias";"and";"BEGIN";"begin";"break";"case";"class";
     "def";"defined";"do";"else";"elsif";"END";"end";"ensure";
     "false";"for";"if";"in";"module";"next";"nil";"not";"or";
     "redo";"rescue";"retry";"return";"self";"super";"then";
     "true";"undef";"unless";"until";"when";"while";"yield";
     "lambda";"proc";"__";"_"]
    Idset.empty

let preamble _ _ usf =
  str "# This extracted Ruby Code\n"
  ++ str "# available at \n"
  ++ (if usf.mldummy then str "def __; end\n" else mt ())
  ++ str "def let(x,&f); f.call(x) end\n"
  ++ str "def match(expr, *fs); tag,args = expr; _,f = fs.assoc(tag); f.call(*args) end\n"
  ++ str "\n"

let pr_id id =
  let s = string_of_id id in
  for i = 0 to String.length s - 1 do
    if s.[i] = '\'' then s.[i] <- '~'
  done;
  str s

let paren = pp_par true
let brace   st = str "{" ++ st ++ str "}"
let bracket st = str "[" ++ st ++ str "]"
let pipe    st = str "|" ++ st ++ str "|"
let def name st =
  str "def " ++ name ++ fnl () ++ (hov 4 st) ++ str "\nend"
let let_ name value st =
  str "let(" ++ value ++ str ")" ++ brace (pipe name ++ st)

let rec pp_abst st = function
  | [] -> st
  | arg::args ->
      str "lambda" ++
	brace (pipe (pr_id arg) ++ pp_abst st args)

let rec pp_apply st _x = function
  | [] -> st
  | arg::args ->
      hov 2 (pp_apply (st ++ str ".call(" ++ arg ++ str ")") _x args)

let pp_global k r = str (Common.pp_global k r)

(*s Pretty-printing of expressions.  *)

let rec pp_expr env args =
  let apply st = pp_apply st true args in
  function
    | MLrel n ->
	let id = get_db_name n env in apply (pr_id id)
    | MLapp (f,args') ->
	let stl = List.map (pp_expr env []) args' in
        pp_expr env (stl @ args) f
    | MLlam _ as a ->
      	let fl,a' = collect_lams a in
	let fl,env' = push_vars fl env in
	apply (pp_abst (pp_expr env' [] a') (List.rev fl))
    | MLletin (id,a1,a2) ->
	let i,env' = push_vars [id] env in
	apply
	  (hv 0
	     (hov 2
		(let_ (pr_id (List.hd i))
		   (pp_expr env [] a1)
		   (pp_expr env' [] a2))))
    | MLglob r ->
	apply (pp_global Term r)
    | MLcons (i,r,args') ->
	assert (args=[]);
	let st =
	  bracket @@
	    str ":" ++ pp_global Cons r
	  ++ str ","
	  ++ bracket (prlist_with_sep spc (pp_cons_args env) args')
	in
	if i = Coinductive then
	  str "lambda" ++ brace st
	else
	  st
    | MLcase ((i,_),t, pv) ->
	let e =
	  if i <> Coinductive then pp_expr env [] t
	  else pp_expr env [] t ++  str ".call()"
	in
	apply (v 3 (str "match" ++ paren (e ++ str "," ++ pp_pat env pv)))
    | MLfix (i,ids,defs) ->
	let ids',env' = push_vars (List.rev (Array.to_list ids)) env in
      	pp_fix env' i (Array.of_list (List.rev ids'),defs) args
    | MLexn s ->
	(* An [MLexn] may be applied, but I don't really care. *)
	paren (str "error" ++ spc () ++ qs s)
    | MLdummy ->
	str "__" (* An [MLdummy] may be applied, but I don't really care. *)
    | MLmagic a ->
	pp_expr env args a
    | MLaxiom -> paren (str "error \"AXIOM TO BE REALIZED\"")

and pp_cons_args env = function
  | MLcons (i,r,args) when i<>Coinductive ->
      bracket @@
	str ":" ++ pp_global Cons r
      ++ str ","
      ++ bracket (prlist_with_sep spc (pp_cons_args env) args)
  | e -> str "," ++ pp_expr env [] e


and pp_one_pat env (r,ids,t) =
  let ids,env' =
    push_vars (List.rev ids) env in
  let args =
    if ids = [] then mt ()
    else pipe @@ prlist_with_sep (fun _ -> str ",") pr_id (List.rev ids)
  in
    (str ":" ++ (pp_global Cons r)),args, (pp_expr env' [] t)

and pp_pat env pv =
  prvect_with_sep (fun _ -> str "," ++ fnl ())
    (fun x -> let s1,s2,s3 = pp_one_pat env x in
       hov 2 (bracket (s1 ++ str ", lambda" ++ brace (s2 ++ s3))))
    pv

(*s names of the functions ([ids]) are already pushed in [env],
    and passed here just for convenience. *)

and pp_fix env j (ids,bl) args =
    paren
      (str "letrec " ++
       (v 0 (paren
	       (prvect_with_sep fnl
		  (fun (fi,ti) ->
		     paren ((pr_id fi) ++ spc () ++ (pp_expr env [] ti)))
		  (array_map2 (fun id b -> (id,b)) ids bl)) ++
	     fnl () ++
      	     hov 2 (pp_apply (pr_id (ids.(j))) true args))))

(*s Pretty-printing of a declaration. *)

let pp_decl = function
  | Dind _ -> mt ()
  | Dtype _ -> mt ()
  | Dfix (rv, defs,_) ->
      let ppv = Array.map (pp_global Term) rv in
      prvect_with_sep fnl
	(fun (pi,ti) ->
	   hov 2
	     ((def pi
		 (pp_expr (empty_env ()) [] ti))
	      ++ fnl ()))
	(array_map2 (fun p b -> (p,b)) ppv defs) ++
      fnl ()
  | Dterm (r, a, _) ->
      if is_inline_custom r then mt ()
      else
	if is_custom r then
	  hov 2 ((def (pp_global Term r) (str (find_custom r))))
	  ++ fnl () ++ fnl ()
	else
	  hov 2 ((def (pp_global Term r) (pp_expr (empty_env ()) [] a)))
	  ++ fnl () ++ fnl ()

let pp_structure_elem = function
  | (l,SEdecl d) -> pp_decl d
  | (l,SEmodule m) ->
      failwith "TODO: Scheme extraction of modules not implemented yet"
  | (l,SEmodtype m) ->
      failwith "TODO: Scheme extraction of modules not implemented yet"

let pp_struct =
  let pp_sel (mp,sel) =
    push_visible mp None;
    let p = prlist_strict pp_structure_elem sel in
    pop_visible (); p
  in
  prlist_strict pp_sel

let ruby_descr = {
  keywords = keywords;
  file_suffix = ".rb";
  capital_file = false;
  preamble = preamble;
  pp_struct = pp_struct;
  sig_suffix = None;
  sig_preamble = (fun _ _ _ -> mt ());
  pp_sig = (fun _ -> mt ());
  pp_decl = pp_decl;
}
