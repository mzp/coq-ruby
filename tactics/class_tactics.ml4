(* -*- compile-command: "make -C .. bin/coqtop.byte" -*- *)
(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, * CNRS-Ecole Polytechnique-INRIA Futurs-Universite Paris Sud *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*i camlp4deps: "parsing/grammar.cma" i*)

(* $Id: class_tactics.ml4 12189 2009-06-15 05:08:44Z msozeau $ *)

open Pp
open Util
open Names
open Nameops
open Term
open Termops
open Sign
open Reduction
open Proof_type
open Proof_trees
open Declarations
open Tacticals
open Tacmach
open Evar_refiner
open Tactics
open Pattern
open Clenv
open Auto
open Rawterm
open Hiddentac
open Typeclasses
open Typeclasses_errors
open Classes
open Topconstr
open Pfedit
open Command
open Libnames
open Evd

let default_eauto_depth = 100
let typeclasses_db = "typeclass_instances"

let _ = Auto.auto_init := (fun () -> 
  Auto.create_hint_db false typeclasses_db full_transparent_state true)

let check_required_library d =
  let d' = List.map id_of_string d in
  let dir = make_dirpath (List.rev d') in
  if not (Library.library_is_loaded dir) then
    error ("Library "^(list_last d)^" has to be required first.")
      
let classes_dirpath =
  make_dirpath (List.map id_of_string ["Classes";"Coq"])
  
let init_setoid () =
  if is_dirpath_prefix_of classes_dirpath (Lib.cwd ()) then ()
  else check_required_library ["Coq";"Setoids";"Setoid"]

(** Typeclasses instance search tactic / eauto *)

let intersects s t =
  Intset.exists (fun el -> Intset.mem el t) s

open Auto

let e_give_exact flags c gl = 
  let t1 = (pf_type_of gl c) and t2 = pf_concl gl in 
    if occur_existential t1 or occur_existential t2 then 
      tclTHEN (Clenvtac.unify (* ~flags *) t1) (exact_no_check c) gl
    else exact_check c gl
(*   let t1 = (pf_type_of gl c) in *)
(*     tclTHEN (Clenvtac.unify ~flags t1) (exact_check c) gl *)
      
let assumption flags id = e_give_exact flags (mkVar id)

open Unification

let auto_unif_flags = {
  modulo_conv_on_closed_terms = Some full_transparent_state;
  use_metas_eagerly = true;
  modulo_delta = var_full_transparent_state;
}

let unify_e_resolve flags (c,clenv) gls = 
  let clenv' = connect_clenv gls clenv in
  let clenv' = clenv_unique_resolver false ~flags clenv' gls 
  in
    Clenvtac.clenv_refine true ~with_classes:false clenv' gls
      
let unify_resolve flags (c,clenv) gls = 
  let clenv' = connect_clenv gls clenv in
  let clenv' = clenv_unique_resolver false ~flags clenv' gls 
  in
    Clenvtac.clenv_refine false ~with_classes:false clenv' gls

let flags_of_state st =
  {auto_unif_flags with 
    modulo_conv_on_closed_terms = Some st; modulo_delta = st}

let rec e_trivial_fail_db db_list local_db goal =
  let tacl = 
    Eauto.registered_e_assumption ::
    (tclTHEN Tactics.intro 
       (function g'->
	  let d = pf_last_hyp g' in
	  let hintl = make_resolve_hyp (pf_env g') (project g') d in
          (e_trivial_fail_db db_list
	      (Hint_db.add_list hintl local_db) g'))) ::
    (List.map pi1 (e_trivial_resolve db_list local_db (pf_concl goal)) )
  in 
  tclFIRST (List.map tclCOMPLETE tacl) goal 

and e_my_find_search db_list local_db hdc concl = 
  let hdc = head_of_constr_reference hdc in
  let hintl =
    list_map_append
      (fun db -> 
	if Hint_db.use_dn db then 
	  let flags = flags_of_state (Hint_db.transparent_state db) in
	    List.map (fun x -> (flags, x)) (Hint_db.map_auto (hdc,concl) db)
	else
	  let flags = flags_of_state (Hint_db.transparent_state db) in
	    List.map (fun x -> (flags, x)) (Hint_db.map_all hdc db))
      (local_db::db_list)
  in 
  let tac_of_hint = 
    fun (flags, {pri=b; pat = p; code=t}) -> 
      let tac =
	match t with
	  | Res_pf (term,cl) -> unify_resolve flags (term,cl)
	  | ERes_pf (term,cl) -> unify_e_resolve flags (term,cl)
	  | Give_exact (c) -> e_give_exact flags c
	  | Res_pf_THEN_trivial_fail (term,cl) ->
              tclTHEN (unify_e_resolve flags (term,cl)) 
		(e_trivial_fail_db db_list local_db)
	  | Unfold_nth c -> unfold_in_concl [all_occurrences,c]
	  | Extern tacast -> conclPattern concl p tacast
      in 
	(tac,b,pr_autotactic t)
  in 
    List.map tac_of_hint hintl

and e_trivial_resolve db_list local_db gl = 
  try 
    e_my_find_search db_list local_db 
      (fst (head_constr_bound gl)) gl
  with Bound | Not_found -> []

let e_possible_resolve db_list local_db gl =
  try 
    e_my_find_search db_list local_db 
      (fst (head_constr_bound gl)) gl
  with Bound | Not_found -> []

let find_first_goal gls = 
  try first_goal gls with UserError _ -> assert false
    
type search_state = { 
  depth : int; (*r depth of search before failing *)
  tacres : goal list sigma * validation;
  pri : int;
  last_tactic : std_ppcmds;
  dblist : Auto.hint_db list;
  localdb : (bool ref * bool ref option * Auto.hint_db) list }
    
let filter_hyp t = 
  match kind_of_term t with
    | Evar _ | Meta _ | Sort _ -> false
    | _ -> true

let rec catchable = function
  | Refiner.FailError _ -> true
  | Stdpp.Exc_located (_, e) -> catchable e
  | e -> Logic.catchable_exception e

let is_dep gl gls =
  let evs = Evarutil.evars_of_term gl.evar_concl in
    if evs = Intset.empty then false
    else
      List.fold_left
	(fun b gl -> 
	  if b then b 
	  else
	    let evs' = Evarutil.evars_of_term gl.evar_concl in
	      intersects evs evs')
	false gls

module SearchProblem = struct
    
  type state = search_state

  let debug = ref false

  let success s = sig_it (fst s.tacres) = []

  let pr_ev evs ev = Printer.pr_constr_env (Evd.evar_env ev) (Evarutil.nf_evar evs ev.Evd.evar_concl)
    
  let pr_goals gls =
    let evars = Evarutil.nf_evars (Refiner.project gls) in
      prlist (pr_ev evars) (sig_it gls)
	
  let filter_tactics (glls,v) l =
    let glls,nv = apply_tac_list Refiner.tclNORMEVAR glls in
    let v p = v (nv p) in
    let rec aux = function
      | [] -> []
      | (tac,pri,pptac) :: tacl -> 
	  try 
	    let (lgls,ptl) = apply_tac_list tac glls in 
	    let v' p = v (ptl p) in
	      ((lgls,v'),pri,pptac) :: aux tacl
	  with e when catchable e -> aux tacl
    in aux l
      
  let nb_empty_evars s = 
    Evd.fold (fun ev evi acc -> if evi.evar_body = Evar_empty then succ acc else acc) s 0

  (* Ordering of states is lexicographic on depth (greatest first) then
     priority (lowest pri means higher priority), then number of remaining goals. *)
  let compare s s' =
    let d = s'.depth - s.depth in
    let nbgoals s = 
      List.length (sig_it (fst s.tacres)) + 
	nb_empty_evars (sig_sig (fst s.tacres))
    in
      if d <> 0 then d else
	let pri = s.pri - s'.pri in
	  if pri <> 0 then pri
	  else nbgoals s - nbgoals s'
	  
  let branching s = 
    if s.depth = 0 then 
      []
    else      
      let (cut, do_cut, ldb as hdldb) = List.hd s.localdb in
	if !cut then
(* 	  let {it=gls; sigma=sigma} = fst s.tacres in *)
(* 	    msg (str"cut:" ++ pr_ev sigma (List.hd gls) ++ str"\n"); *)
	    []
	else begin
	  let {it=gl; sigma=sigma} = fst s.tacres in 
	    Option.iter (fun r ->
(*  	      msg (str"do cut:" ++ pr_ev sigma (List.hd gl) ++ str"\n"); *)
	      r := true) do_cut;
	  let gl = List.map (Evarutil.nf_evar_info sigma) gl in
	  let nbgl = List.length gl in
(* 	  let gl' = { it = gl ; sigma = sigma } in *)
(* 	  let tacres' = gl', snd s.tacres in *)
	  let new_db, localdb = 
	    let tl = List.tl s.localdb in
	      match tl with
	      | [] -> hdldb, tl
	      | (cut', do', ldb') :: rest ->
		  if not (is_dep (List.hd gl) (List.tl gl)) then
		    let fresh = ref false in
		      if do' = None then (
(*  			msg (str"adding a cut:" ++ pr_ev sigma (List.hd gl) ++ str"\n"); *)
			(fresh, None, ldb), (cut', Some fresh, ldb') :: rest
		      ) else (
(* 			msg (str"keeping the previous cut:" ++ pr_ev sigma (List.hd gl) ++ str"\n"); *)
			(cut', None, ldb), tl )
		  else hdldb, tl
	  in let localdb = new_db :: localdb in
	  let intro_tac =
	    List.map
	      (fun ((lgls,_) as res,pri,pp) ->
		let g' = first_goal lgls in
		let hintl =
		  make_resolve_hyp (pf_env g') (project g') (pf_last_hyp g')
		in
		let ldb = Hint_db.add_list hintl ldb in
		  { s with tacres = res;
		    last_tactic = pp;
		    pri = pri;
		    localdb = (cut, None, ldb) :: List.tl s.localdb })
	      (filter_tactics s.tacres [Tactics.intro,1,(str "intro")])
	  in
	  let possible_resolve ((lgls,_) as res, pri, pp) =
	    let nbgl' = List.length (sig_it lgls) in
	      if nbgl' < nbgl then
		{ s with 
		  depth = pred s.depth;
		  tacres = res; last_tactic = pp; pri = pri;
		  localdb = List.tl localdb }
	      else 
		{ s with depth = pred s.depth; tacres = res; 
		  last_tactic = pp; pri = pri;
		  localdb = list_tabulate (fun _ -> new_db) (nbgl'-nbgl) @ localdb }
	  in
	  let rec_tacs = 
	    let l = 
	      filter_tactics s.tacres (e_possible_resolve s.dblist ldb (List.hd gl).evar_concl)
	    in
	      List.map possible_resolve l
	  in
	    List.sort compare (intro_tac @ rec_tacs)
	end
	  
  let pp s = 
    msg (hov 0 (str " depth=" ++ int s.depth ++ spc () ++ 
		  s.last_tactic ++ str "\n"))

end

module Search = Explore.Make(SearchProblem)

let make_initial_state n gls dblist localdbs =
  { depth = n;
    tacres = gls;
    pri = 0;
    last_tactic = (mt ());
    dblist = dblist;
    localdb = localdbs }

let e_depth_search debug s =
  let tac = if debug then
    (SearchProblem.debug := true; Search.debug_depth_first) else Search.depth_first in
  let s = tac s in
    s.tacres

let e_breadth_search debug s =
  try
    let tac = 
      if debug then Search.debug_breadth_first else Search.breadth_first 
    in let s = tac s in s.tacres
  with Not_found -> error "eauto: breadth first search failed."


(* A special one for getting everything into a dnet. *)

let is_transparent_gr (ids, csts) = function
  | VarRef id -> Idpred.mem id ids
  | ConstRef cst -> Cpred.mem cst csts
  | IndRef _ | ConstructRef _ -> false

let make_resolve_hyp env sigma st flags pri (id, _, cty) =
  let cty = Evarutil.nf_evar sigma cty in
  let ctx, ar = decompose_prod cty in
  let keep = 
    match kind_of_term (fst (decompose_app ar)) with
    | Const c -> is_class (ConstRef c)
    | Ind i -> is_class (IndRef i)
    | _ -> false
  in
    if keep then let c = mkVar id in
      map_succeed 
	(fun f -> f (c,cty)) 
	[make_exact_entry pri; make_apply_entry env sigma flags pri]
    else []

let make_local_hint_db st eapply lems g =
  let sign = pf_hyps g in
  let hintlist = list_map_append (pf_apply make_resolve_hyp g st (eapply,false,false) None) sign in
  let hintlist' = list_map_append (pf_apply make_resolves g (eapply,false,false) None) lems in
    Hint_db.add_list hintlist' (Hint_db.add_list hintlist (Hint_db.empty st true))

let e_search_auto debug (in_depth,p) lems st db_list gls = 
  let sigma = Evd.sig_sig (fst gls) and gls' = Evd.sig_it (fst gls) in
  let local_dbs = List.map (fun gl -> 
    let db = make_local_hint_db st true lems ({it = gl; sigma = sigma}) in
      (ref false, None, db)) gls' in
  let state = make_initial_state p gls db_list local_dbs in
  if in_depth then 
    e_depth_search debug state
  else
    e_breadth_search debug state

let full_eauto debug n lems gls = 
  let dbnames = current_db_names () in
  let dbnames =  list_subtract dbnames ["v62"] in
  let db_list = List.map searchtable_map dbnames in
  let db = searchtable_map typeclasses_db in
    e_search_auto debug n lems (Hint_db.transparent_state db) db_list gls

let nf_goal (gl, valid) =
  { gl with sigma = Evarutil.nf_evars gl.sigma }, valid

let typeclasses_eauto debug n lems gls =
  let db = searchtable_map typeclasses_db in
    e_search_auto debug n lems (Hint_db.transparent_state db) [db] gls

exception Found of evar_map

let valid goals p res_sigma l = 
  let evm = 
    List.fold_left2 
      (fun sigma (ev, evi) prf ->
	let cstr, obls = Refiner.extract_open_proof !res_sigma prf in
	  if not (Evd.is_defined sigma ev) then
	    Evd.define sigma ev cstr
	  else sigma)
      !res_sigma goals l
  in raise (Found evm)

let is_dependent ev evm = 
  Evd.fold (fun ev' evi dep -> 
    if ev = ev' then dep
    else dep || occur_evar ev evi.evar_concl)
    evm false
    
let resolve_all_evars_once debug (mode, depth) env p evd =
  let evm = Evd.evars_of evd in
  let goals, evm' = 
    Evd.fold
      (fun ev evi (gls, evm') ->
	if evi.evar_body = Evar_empty 
	  && Typeclasses.is_resolvable evi
(* 	  && not (is_dependent ev evm) *)
	  && p ev evi then ((ev,evi) :: gls, Evd.add evm' ev (Typeclasses.mark_unresolvable evi)) else 
	  (gls, Evd.add evm' ev evi))
      evm ([], Evd.empty)
  in
  let goals = List.rev goals in
  let gls = { it = List.map snd goals; sigma = evm' } in
  let res_sigma = ref evm' in
  let gls', valid' = typeclasses_eauto debug (mode, depth) [] (gls, valid goals p res_sigma) in
    res_sigma := Evarutil.nf_evars (sig_sig gls');
    try ignore(valid' []); assert(false) 
    with Found evm' -> Evarutil.nf_evar_defs (Evd.evars_reset_evd evm' evd)

exception FoundTerm of constr

let resolve_one_typeclass env ?(sigma=Evd.empty) gl =
  let gls = { it = [ Evd.make_evar (Environ.named_context_val env) gl ] ; sigma = sigma } in
  let valid x = raise (FoundTerm (fst (Refiner.extract_open_proof sigma (List.hd x)))) in
  let gls', valid' = typeclasses_eauto false (true, default_eauto_depth) [] (gls, valid) in
    try ignore(valid' []); assert false with FoundTerm t -> 
      let term = Evarutil.nf_evar (sig_sig gls') t in
	if occur_existential term then raise Not_found else term

let _ = 
  Typeclasses.solve_instanciation_problem := (fun x y z -> resolve_one_typeclass x ~sigma:y z)

let has_undefined p oevd evd =
  Evd.fold (fun ev evi has -> has ||
    (evi.evar_body = Evar_empty && p ev evi && 
	(try Typeclasses.is_resolvable (Evd.find oevd ev) with _ -> true)))
    (Evd.evars_of evd) false

let rec merge_deps deps = function
  | [] -> [deps]
  | hd :: tl -> 
      if intersects deps hd then 
	merge_deps (Intset.union deps hd) tl
      else hd :: merge_deps deps tl
	
let split_evars evm =
  Evd.fold (fun ev evi acc ->
    let deps = Intset.union (Intset.singleton ev) (Evarutil.evars_of_term evi.evar_concl) in
      merge_deps deps acc)
    evm []

let select_evars evs evm =
  Evd.fold (fun ev evi acc ->
    if Intset.mem ev evs then Evd.add acc ev evi else acc)
    evm Evd.empty

let resolve_all_evars debug m env p oevd do_split fail =
  let oevm = Evd.evars_of oevd in
  let split = if do_split then split_evars (Evd.evars_of (Evd.undefined_evars oevd)) else [Intset.empty] in
  let p = if do_split then 
    fun comp ev evi -> (Intset.mem ev comp || not (Evd.mem oevm ev)) && p ev evi
    else fun _ -> p 
  in
  let rec aux n p evd =
    if has_undefined p oevm evd then
      if n > 0 then
	let evd' = resolve_all_evars_once debug m env p evd in
	  aux (pred n) p evd'
      else None
    else Some evd
  in 
  let rec docomp evd = function
    | [] -> evd
    | comp :: comps ->
	let res = try aux 3 (p comp) evd with Not_found -> None in
	  match res with
	  | None -> 
	      if fail then 
		(* Unable to satisfy the constraints. *)
		let evm = Evd.evars_of evd in
		let evm = if do_split then select_evars comp evm else evm in
		let _, ev = Evd.fold 
		  (fun ev evi (b,acc) -> 
		    (* focus on one instance if only one was searched for *)
		    if class_of_constr evi.evar_concl <> None then
		      if not b (* || do_split *) then
			true, Some ev 
		      else b, None
		    else b, acc) evm (false, None)
		in
		  Typeclasses_errors.unsatisfiable_constraints env (Evd.evars_reset_evd evm evd) ev
	      else (* Best effort: do nothing *) oevd
	  | Some evd' -> docomp evd' comps
  in docomp oevd split

let resolve_typeclass_evars d p env evd onlyargs split fail =
  let pred = 
    if onlyargs then 
      (fun ev evi -> Typeclasses.is_implicit_arg (snd (Evd.evar_source ev evd)) &&
	Typeclasses.is_class_evar evi)
    else (fun ev evi -> Typeclasses.is_class_evar evi)
  in resolve_all_evars d p env pred evd split fail
    
let solve_inst debug mode depth env evd onlyargs split fail =
  resolve_typeclass_evars debug (mode, depth) env evd onlyargs split fail

let _ = 
  Typeclasses.solve_instanciations_problem :=
    solve_inst false true default_eauto_depth

    
VERNAC COMMAND EXTEND Typeclasses_Unfold_Settings
| [ "Typeclasses" "Transparent" reference_list(cl) ] -> [
    add_hints false [typeclasses_db] (Vernacexpr.HintsTransparency (cl, true))
  ]
END
	
VERNAC COMMAND EXTEND Typeclasses_Rigid_Settings
| [ "Typeclasses" "Opaque" reference_list(cl) ] -> [
    add_hints false [typeclasses_db] (Vernacexpr.HintsTransparency (cl, false))
  ]
END

(** Typeclass-based rewriting. *)

let morphism_class = 
  lazy (class_info (Nametab.global (Qualid (dummy_loc, qualid_of_string "Coq.Classes.Morphisms.Morphism"))))

let morphism_proxy_class = 
  lazy (class_info (Nametab.global (Qualid (dummy_loc, qualid_of_string "Coq.Classes.Morphisms.MorphismProxy"))))

let respect_proj = lazy (mkConst (Option.get (snd (List.hd (Lazy.force morphism_class).cl_projs))))

let make_dir l = make_dirpath (List.map id_of_string (List.rev l))

let try_find_global_reference dir s =
  let sp = Libnames.make_path (make_dir ("Coq"::dir)) (id_of_string s) in
    Nametab.absolute_reference sp
      
let try_find_reference dir s =
  constr_of_global (try_find_global_reference dir s)
    
let gen_constant dir s = Coqlib.gen_constant "Class_setoid" dir s
let coq_proj1 = lazy(gen_constant ["Init"; "Logic"] "proj1")
let coq_proj2 = lazy(gen_constant ["Init"; "Logic"] "proj2")
let coq_eq = lazy(gen_constant ["Init"; "Logic"] "eq")
let iff = lazy (gen_constant ["Init"; "Logic"] "iff")
let coq_all = lazy (gen_constant ["Init"; "Logic"] "all")
let impl = lazy (gen_constant ["Program"; "Basics"] "impl")
let arrow = lazy (gen_constant ["Program"; "Basics"] "arrow")
let coq_id = lazy (gen_constant ["Init"; "Datatypes"] "id")

let reflexive_type = lazy (try_find_reference ["Classes"; "RelationClasses"] "Reflexive")
let reflexive_proof_global = lazy (try_find_global_reference ["Classes"; "RelationClasses"] "reflexivity")
let reflexive_proof = lazy (try_find_reference ["Classes"; "RelationClasses"] "reflexivity")

let symmetric_type = lazy (try_find_reference ["Classes"; "RelationClasses"] "Symmetric")
let symmetric_proof = lazy (try_find_reference ["Classes"; "RelationClasses"] "symmetry")
let symmetric_proof_global = lazy (try_find_global_reference ["Classes"; "RelationClasses"] "symmetry")

let transitive_type = lazy (try_find_reference ["Classes"; "RelationClasses"] "Transitive")
let transitive_proof = lazy (try_find_reference ["Classes"; "RelationClasses"] "transitivity")
let transitive_proof_global = lazy (try_find_global_reference ["Classes"; "RelationClasses"] "transitivity")

let coq_inverse = lazy (gen_constant (* ["Classes"; "RelationClasses"] "inverse" *)
			   ["Program"; "Basics"] "flip")

let inverse car rel = mkApp (Lazy.force coq_inverse, [| car ; car; mkProp; rel |])
(* let inverse car rel = mkApp (Lazy.force coq_inverse, [| car ; car; new_Type (); rel |]) *)

let complement = lazy (gen_constant ["Classes"; "RelationClasses"] "complement")
let forall_relation = lazy (gen_constant ["Classes"; "Morphisms"] "forall_relation")
let pointwise_relation = lazy (gen_constant ["Classes"; "Morphisms"] "pointwise_relation")

let respectful_dep = lazy (gen_constant ["Classes"; "Morphisms"] "respectful_dep")
let respectful = lazy (gen_constant ["Classes"; "Morphisms"] "respectful")

let equivalence = lazy (gen_constant ["Classes"; "RelationClasses"] "Equivalence")
let default_relation = lazy (gen_constant ["Classes"; "SetoidTactics"] "DefaultRelation")

let coq_relation = lazy (gen_constant ["Relations";"Relation_Definitions"] "relation")
let mk_relation a = mkApp (Lazy.force coq_relation, [| a |])
(* let mk_relation a = mkProd (Anonymous, a, mkProd (Anonymous, a, new_Type ())) *)

let coq_relationT = lazy (gen_constant ["Classes";"Relations"] "relationT")

let setoid_refl_proj = lazy (gen_constant ["Classes"; "SetoidClass"] "Equivalence_Reflexive")

let setoid_equiv = lazy (gen_constant ["Classes"; "SetoidClass"] "equiv")
let setoid_morphism = lazy (gen_constant ["Classes"; "SetoidClass"] "setoid_morphism")
let setoid_refl_proj = lazy (gen_constant ["Classes"; "SetoidClass"] "Equivalence_Reflexive")

let setoid_relation = lazy (gen_constant ["Classes"; "SetoidTactics"] "SetoidRelation")
  
let arrow_morphism a b = 
  if isprop a && isprop b then
    Lazy.force impl
  else
    mkApp(Lazy.force arrow, [|a;b|])

let setoid_refl pars x = 
  applistc (Lazy.force setoid_refl_proj) (pars @ [x])
      
let morphism_type = lazy (constr_of_global (Lazy.force morphism_class).cl_impl)

let morphism_proxy_type = lazy (constr_of_global (Lazy.force morphism_proxy_class).cl_impl)

let is_applied_setoid_relation t =
  match kind_of_term t with
  | App (c, args) when Array.length args >= 2 ->
      let head = if isApp c then fst (destApp c) else c in 
	if eq_constr (Lazy.force coq_eq) head then false
	else (try      
	    let evd, evar = Evarutil.new_evar (Evd.create_evar_defs Evd.empty) (Global.env()) (new_Type ()) in
	    let inst = mkApp (Lazy.force setoid_relation, [| evar; c |]) in
	      ignore(Typeclasses.resolve_one_typeclass (Global.env()) (Evd.evars_of evd) inst);
	      true
	  with _ -> false)
  | _ -> false
      
let _ = 
  Equality.register_is_applied_setoid_relation is_applied_setoid_relation

let split_head = function
    hd :: tl -> hd, tl
  | [] -> assert(false)

let build_signature isevars env m (cstrs : 'a option list) (finalcstr : 'a Lazy.t option) (f : 'a -> constr) =
  let new_evar isevars env t =
    Evarutil.e_new_evar isevars env
      (* ~src:(dummy_loc, ImplicitArg (ConstRef (Lazy.force respectful), (n, Some na))) *) t
  in
  let mk_relty ty obj =
    match obj with
      | None -> 
	  let relty = mk_relation ty in
	    new_evar isevars env relty
      | Some x -> f x
  in
  let rec aux env ty l =
    let t = Reductionops.whd_betadeltaiota env (Evd.evars_of !isevars) ty in
      match kind_of_term t, l with
      | Prod (na, ty, b), obj :: cstrs -> 
	  if dependent (mkRel 1) b then
	    let (b, arg, evars) = aux (Environ.push_rel (na, None, ty) env) b cstrs in
	    let ty = Reductionops.nf_betaiota (Evd.evars_of !isevars) ty in
	    let pred = mkLambda (na, ty, b) in
	    let liftarg = mkLambda (na, ty, arg) in
	    let arg' = mkApp (Lazy.force forall_relation, [| ty ; pred ; liftarg |]) in
	      mkProd(na, ty, b), arg', (ty, None) :: evars
	  else
	    let (b', arg, evars) = aux env (subst1 mkProp b) cstrs in
	    let ty = Reductionops.nf_betaiota(Evd.evars_of !isevars) ty in
	    let relty = mk_relty ty obj in
	    let newarg = mkApp (Lazy.force respectful, [| ty ; b' ; relty ; arg |]) in
	      mkProd(na, ty, b), newarg, (ty, Some relty) :: evars
      | _, obj :: _ -> anomaly "build_signature: not enough products"
      | _, [] -> 
	  (match finalcstr with
	      None -> 
		let t = Reductionops.nf_betaiota(Evd.evars_of !isevars) ty in
		let rel = mk_relty t None in 
		  t, rel, [t, Some rel]
	    | Some codom -> let (t, rel) = Lazy.force codom in
			      t, rel, [t, Some rel])
  in aux env m cstrs

let morphism_proof env evars carrier relation x =
  let goal =
    mkApp (Lazy.force morphism_proxy_type, [| carrier ; relation; x |])
  in Evarutil.e_new_evar evars env goal

let find_class_proof proof_type proof_method env evars carrier relation =
  try 
    let goal = mkApp (Lazy.force proof_type, [| carrier ; relation |]) in
      Typeclasses.resolve_one_typeclass env evars goal
  with e when Logic.catchable_exception e -> raise Not_found

let get_reflexive_proof env = find_class_proof reflexive_type reflexive_proof env
let get_symmetric_proof env = find_class_proof symmetric_type symmetric_proof env
let get_transitive_proof env = find_class_proof transitive_type transitive_proof env

exception FoundInt of int

let array_find (arr: 'a array) (pred: int -> 'a -> bool): int = 
  try
    for i=0 to Array.length arr - 1 do if pred i (arr.(i)) then raise (FoundInt i) done;
    raise Not_found
  with FoundInt i -> i

let resolve_morphism env sigma oldt m ?(fnewt=fun x -> x) args args' cstr evars =
  let morph_instance, proj, sigargs, m', args, args' = 
    let first = try (array_find args' (fun i b -> b <> None)) with Not_found -> raise (Invalid_argument "resolve_morphism") in
    let morphargs, morphobjs = array_chop first args in
    let morphargs', morphobjs' = array_chop first args' in
    let appm = mkApp(m, morphargs) in
    let appmtype = Typing.type_of env sigma appm in
    let cstrs = List.map (function None -> None | Some (_, (a, r, _, _)) -> Some (a, r)) (Array.to_list morphobjs') in
    let appmtype', signature, sigargs = build_signature evars env appmtype cstrs cstr (fun (a, r) -> r) in
    let cl_args = [| appmtype' ; signature ; appm |] in
    let app = mkApp (Lazy.force morphism_type, cl_args) in
    let morph = Evarutil.e_new_evar evars env app in
      morph, morph, sigargs, appm, morphobjs, morphobjs'
  in 
  let projargs, respars, typeargs = 
    array_fold_left2 
      (fun (acc, sigargs, typeargs') x y -> 
	let (carrier, relation), sigargs = split_head sigargs in
	  match relation with
	  | Some relation ->
	      (match y with
	      | None ->
		  let proof = morphism_proof env evars carrier relation x in
		    [ proof ; x ; x ] @ acc, sigargs, x :: typeargs'
	      | Some (p, (_, _, _, t')) ->
		  [ p ; t'; x ] @ acc, sigargs, t' :: typeargs')
	  | None -> 
	      if y <> None then error "Cannot rewrite the argument of a dependent function";
	      x :: acc, sigargs, x :: typeargs')
      ([], sigargs, []) args args'
  in
  let proof = applistc proj (List.rev projargs) in
  let newt = applistc m' (List.rev typeargs) in
    match respars with
	[ a, Some r ] -> (proof, (a, r, oldt, fnewt newt))
      | _ -> assert(false)

(* Adapted from setoid_replace. *)

type hypinfo = {
  cl : clausenv;
  prf : constr;
  car : constr;
  rel : constr;
  l2r : bool;
  c1 : constr;
  c2 : constr;
  c  : constr option;
  abs : (constr * types) option;
}

let evd_convertible env evd x y =
  try ignore(Evarconv.the_conv_x env x y evd); true 
  with _ -> false
  
let decompose_setoid_eqhyp env sigma c left2right =
  let ctype = Typing.type_of env sigma c in
  let find_rel ty = 
    let eqclause = Clenv.mk_clenv_from_env env sigma None (c,ty) in
    let (equiv, args) = decompose_app (Clenv.clenv_type eqclause) in
    let rec split_last_two = function
      | [c1;c2] -> [],(c1, c2)
      | x::y::z ->
	  let l,res = split_last_two (y::z) in x::l, res
      | _ -> error "The term provided is not an applied relation." in
    let others,(c1,c2) = split_last_two args in
    let ty1, ty2 = 
      Typing.mtype_of env eqclause.evd c1, Typing.mtype_of env eqclause.evd c2
    in
      if not (evd_convertible env eqclause.evd ty1 ty2) then None
      else
	Some { cl=eqclause; prf=(Clenv.clenv_value eqclause);
	       car=ty1; rel=mkApp (equiv, Array.of_list others);
	       l2r=left2right; c1=c1; c2=c2; c=Some c; abs=None }
  in
    match find_rel ctype with
    | Some c -> c
    | None -> 
	let ctx,t' = Reductionops.splay_prod_assum env sigma ctype in (* Search for underlying eq *)
	match find_rel (it_mkProd_or_LetIn t' ctx) with
	| Some c -> c
	| None -> error "The term does not end with an applied homogeneous relation."
	    
let rewrite_unif_flags = {
  Unification.modulo_conv_on_closed_terms = None;
  Unification.use_metas_eagerly = true;
  Unification.modulo_delta = empty_transparent_state;
}

let conv_transparent_state = (Idpred.empty, Cpred.full)

let rewrite2_unif_flags = {
  Unification.modulo_conv_on_closed_terms = Some conv_transparent_state;
  Unification.use_metas_eagerly = true;
  Unification.modulo_delta = empty_transparent_state;
}

let convertible env evd x y =
  Reductionops.is_conv env (Evd.evars_of evd) x y
  
let allowK = true

let refresh_hypinfo env sigma hypinfo = 
  if !hypinfo.abs = None then
    let {l2r=l2r; c = c;cl=cl} = !hypinfo in
      match c with 
	| Some c ->
	    (* Refresh the clausenv to not get the same meta twice in the goal. *)
	    hypinfo := decompose_setoid_eqhyp env (Evd.evars_of cl.evd) c l2r;
	| _ -> ()
  else ()

let unify_eqn env sigma hypinfo t = 
  if isEvar t then None
  else try 
    let {cl=cl; prf=prf; car=car; rel=rel; l2r=l2r; c1=c1; c2=c2; c=c; abs=abs} = !hypinfo in
    let env', prf, c1, c2, car, rel =
      let left = if l2r then c1 else c2 in
	match abs with
	    Some (absprf, absprfty) -> 
	      let env' = clenv_unify allowK ~flags:rewrite2_unif_flags CONV left t cl in
		env', prf, c1, c2, car, rel
	  | None ->
	      let env' =
		try clenv_unify allowK ~flags:rewrite_unif_flags
		  CONV left t cl
		with Pretype_errors.PretypeError _ ->
		  (* For Ring essentially, only when doing setoid_rewrite *)
		  clenv_unify allowK ~flags:rewrite2_unif_flags
		    CONV left t cl
	      in
	      let env' = 
		let mvs = clenv_dependent false env' in
		  clenv_pose_metas_as_evars env' mvs
	      in
	      let evd' = Typeclasses.resolve_typeclasses ~fail:false env'.env env'.evd in
	      let env' = { env' with evd = evd' } in
	      let nf c = Evarutil.nf_evar (Evd.evars_of evd') (Clenv.clenv_nf_meta env' c) in
	      let c1 = nf c1 and c2 = nf c2
	      and car = nf car and rel = nf rel 
	      and prf = nf (Clenv.clenv_value env') in
	      let ty1 = Typing.mtype_of env'.env env'.evd c1 
	      and ty2 = Typing.mtype_of env'.env env'.evd c2
	      in
		if convertible env env'.evd ty1 ty2 then (
		  if occur_meta prf then refresh_hypinfo env sigma hypinfo;
		  env', prf, c1, c2, car, rel)
		else raise Reduction.NotConvertible
    in
    let res =
      if l2r then (prf, (car, rel, c1, c2))
      else
	try (mkApp (get_symmetric_proof env Evd.empty car rel, [| c1 ; c2 ; prf |]), (car, rel, c2, c1))
	with Not_found ->
	  (prf, (car, inverse car rel, c2, c1))
    in Some (env', res)
  with e when catchable e -> None

let unfold_impl t =
  match kind_of_term t with
    | App (arrow, [| a; b |])(*  when eq_constr arrow (Lazy.force impl) *) -> 
	mkProd (Anonymous, a, lift 1 b)
    | _ -> assert false

let unfold_id t = 
  match kind_of_term t with
    | App (id, [| a; b |]) (* when eq_constr id (Lazy.force coq_id) *) -> b
    | _ -> assert false

let unfold_all t = 
  match kind_of_term t with
    | App (id, [| a; b |]) (* when eq_constr id (Lazy.force coq_all) *) ->
	(match kind_of_term b with
	  | Lambda (n, ty, b) -> mkProd (n, ty, b)
	  | _ -> assert false)
    | _ -> assert false

let decomp_prod env evm n c = 
  snd (Reductionops.decomp_n_prod env evm n c)

let rec decomp_pointwise n c =
  if n = 0 then c
  else
    match kind_of_term c with
      | App (pointwise, [| a; b; relb |]) -> decomp_pointwise (pred n) relb
      | _ -> raise Not_found
	  
let lift_cstr env sigma evars args cstr =
  let cstr () =
    let start = 
      match cstr with
	| Some codom -> Lazy.force codom
	| None -> let car = Evarutil.e_new_evar evars env (new_Type ()) in
	  let rel = Evarutil.e_new_evar evars env (mk_relation car) in
	    (car, rel)
    in
      Array.fold_right
	(fun arg (car, rel) -> 
	  let ty = Typing.type_of env sigma arg in
	  let car' = mkProd (Anonymous, ty, car) in
	  let rel' = mkApp (Lazy.force pointwise_relation, [| ty; car; rel |]) in
	    (car', rel'))
	args start
  in Some (Lazy.lazy_from_fun cstr)

let unlift_cstr env sigma = function
  | None -> None
  | Some codom -> 
      let cstr () =
	let car, rel = Lazy.force codom in
	  decomp_prod env sigma 1 car, decomp_pointwise 1 rel
      in Some (Lazy.lazy_from_fun cstr)

type rewrite_flags = { under_lambdas : bool; on_morphisms : bool }

let default_flags = { under_lambdas = true; on_morphisms = true; }

let build_new gl env sigma flags loccs hypinfo concl cstr evars =
  let (nowhere_except_in,occs) = loccs in
  let is_occ occ = 
    if nowhere_except_in then List.mem occ occs else not (List.mem occ occs) in
  let rec aux env t occ cstr =
    let unif = unify_eqn env sigma hypinfo t in
    let occ = if unif = None then occ else succ occ in
      match unif with
      | Some (env', (prf, hypinfo as x)) when is_occ occ -> 
	  begin 
	    evars := Evd.evar_merge !evars 
	      (Evd.evars_of (Evd.undefined_evars (Evarutil.nf_evar_defs env'.evd)));
	    match cstr with
	    | None -> Some x, occ
	    | Some _ ->
		let (car, r, orig, dest) = hypinfo in
		let res = 
		  resolve_morphism env sigma t ~fnewt:unfold_id
		    (mkApp (Lazy.force coq_id, [| car |]))
		    [| orig |] [| Some x |] cstr evars
		in Some res, occ
	  end
      | _ -> 
	  match kind_of_term t with
	  | App (m, args) ->
	      let rewrite_args occ = 
		let args', occ = 
		  Array.fold_left 
		    (fun (acc, occ) arg -> let res, occ = aux env arg occ None in (res :: acc, occ))
		    ([], occ) args
		in
		let res =
		  if List.for_all (fun x -> x = None) args' then None
		  else 
		    let args' = Array.of_list (List.rev args') in
		      (Some (resolve_morphism env sigma t m args args' cstr evars))
		in res, occ
	      in
		if flags.on_morphisms then
		  let m', occ = aux env m occ (lift_cstr env sigma evars args cstr) in
		    match m' with
		    | None -> rewrite_args occ (* Standard path, try rewrite on arguments *)
		    | Some (prf, (car, rel, c1, c2)) -> 
			(* We rewrote the function and get a proof of pointwise rel for the arguments.
			   We just apply it. *)
			let nargs = Array.length args in
			let res = 
			  mkApp (prf, args),
			  (decomp_prod env (Evd.evars_of !evars) nargs car, 
			  decomp_pointwise nargs rel, mkApp(c1, args), mkApp(c2, args))
			in Some res, occ
		else rewrite_args occ
		
	  | Prod (n, x, b) when not (dependent (mkRel 1) b) -> 
	      let x', occ = aux env x occ None in
(* 		if x' = None && flags.under_lambdas then *)
(* 		  let lam = mkLambda (n, x, b) in *)
(* 		  let lam', occ = aux env lam occ None in *)
(* 		  let res =  *)
(* 		    match lam' with *)
(* 		    | None -> None *)
(* 		    | Some (prf, (car, rel, c1, c2)) -> *)
(* 			Some (resolve_morphism env sigma t *)
(* 				 ~fnewt:unfold_all *)
(* 				 (Lazy.force coq_all) [| x ; lam |] [| None; lam' |] *)
(* 				 cstr evars) *)
(* 		  in res, occ *)
(* 		else *)
		  let b = subst1 mkProp b in
		  let b', occ = aux env b occ None in
		  let res = 
		    if x' = None && b' = None then None
		    else 
		      Some (resolve_morphism env sigma t
			       ~fnewt:unfold_impl
			       (arrow_morphism (Typing.type_of env sigma x) (Typing.type_of env sigma b))
			       [| x ; b |] [| x' ; b' |]
			       cstr evars)
		  in res, occ
		    
	  | Prod (n, ty, b) ->
	      let lam = mkLambda (n, ty, b) in
	      let lam', occ = aux env lam occ None in
	      let res = 
		match lam' with
		| None -> None
		| Some (prf, (car, rel, c1, c2)) ->
		    Some (resolve_morphism env sigma t
			     ~fnewt:unfold_all
			     (Lazy.force coq_all) [| ty ; lam |] [| None; lam' |]
			     cstr evars)
	      in res, occ
		
	  | Lambda (n, t, b) when flags.under_lambdas ->
	      let env' = Environ.push_rel (n, None, t) env in
		refresh_hypinfo env' sigma hypinfo;
		let b', occ = aux env' b occ (unlift_cstr env sigma cstr) in
		let res =
		  match b' with
		  | None -> None
		  | Some (prf, (car, rel, c1, c2)) ->
		      let prf' = mkLambda (n, t, prf) in
		      let car' = mkProd (n, t, car) in
		      let rel' = mkApp (Lazy.force pointwise_relation, [| t; car; rel |]) in
		      let c1' = mkLambda(n, t, c1) and c2' = mkLambda (n, t, c2) in
			Some (prf', (car', rel', c1', c2'))
		in res, occ
	  | _ -> None, occ
  in
  let eq,nbocc_min_1 = aux env concl 0 cstr in
  let rest = List.filter (fun o -> o > nbocc_min_1) occs in
  if rest <> [] then error_invalid_occurrence rest;
  eq
          
let cl_rewrite_clause_aux ?(flags=default_flags) hypinfo goal_meta occs clause gl =
  let concl, is_hyp = 
    match clause with
	Some ((_, id), _) -> pf_get_hyp_typ gl id, Some id
      | None -> pf_concl gl, None
  in
  let cstr = 
    let sort = mkProp in
    let impl = Lazy.force impl in
      match is_hyp with
      | None -> (sort, inverse sort impl)
      | Some _ -> (sort, impl)
  in
  let sigma = project gl in
  let evars = ref (Evd.create_evar_defs sigma) in
  let env = pf_env gl in
  let eq = build_new gl env sigma flags occs hypinfo concl (Some (Lazy.lazy_from_val cstr)) evars 
  in
    match eq with
    | Some (p, (_, _, oldt, newt)) -> 
	(try 
	  evars := Typeclasses.resolve_typeclasses env ~split:false ~fail:true !evars;
	  let p = Evarutil.nf_isevar !evars p in
	  let newt = Evarutil.nf_isevar !evars newt in
	  let undef = Evd.undefined_evars !evars in
	  let abs = Option.map (fun (x, y) -> Evarutil.nf_isevar !evars x,
	    Evarutil.nf_isevar !evars y) !hypinfo.abs in
	  let rewtac = 
	    match is_hyp with
	    | Some id -> 
		let term = 
		  match abs with
		  | None -> p
		  | Some (t, ty) -> 
		      mkApp (mkLambda (Name (id_of_string "lemma"), ty, p), [| t |])
		in
		  cut_replacing id newt 
		    (fun x -> Tacmach.refine_no_check (mkApp (term, [| mkVar id |])))
	    | None -> 
		(match abs with
		| None -> 
		    let name = next_name_away_with_default "H" Anonymous (pf_ids_of_hyps gl) in
		      tclTHENLAST
			(Tacmach.internal_cut_no_check false name newt)
			(tclTHEN (Tactics.revert [name]) (Tacmach.refine_no_check p))
		| Some (t, ty) -> 
		    Tacmach.refine_no_check
		      (mkApp (mkLambda (Name (id_of_string "newt"), newt,
				       mkLambda (Name (id_of_string "lemma"), ty,
						mkApp (p, [| mkRel 2 |]))),
			     [| mkMeta goal_meta; t |])))
	  in
	  let evartac =
	    let evd = Evd.evars_of undef in
	      if not (evd = Evd.empty) then Refiner.tclEVARS (Evd.merge sigma evd)
	      else tclIDTAC
	  in tclTHENLIST [evartac; rewtac] gl
	  with 
	  | Stdpp.Exc_located (_, TypeClassError (env, (UnsatisfiableConstraints _ as e)))
	  | TypeClassError (env, (UnsatisfiableConstraints _ as e)) ->
	      tclFAIL 0 (str" setoid rewrite failed: unable to satisfy the rewriting constraints." 
			  ++ fnl () ++ Himsg.explain_typeclass_error env e) gl)
	  (* 	      | Not_found -> *)
	  (* 		  tclFAIL 0 (str" setoid rewrite failed: unable to satisfy the rewriting constraints.") gl) *)
    | None -> 
	let {l2r=l2r; c1=x; c2=y} = !hypinfo in
	  raise (Pretype_errors.PretypeError 
		    (pf_env gl, 
		    Pretype_errors.NoOccurrenceFound ((if l2r then x else y), is_hyp)))
	    (* tclFAIL 1 (str"setoid rewrite failed") gl *)
	  
let cl_rewrite_clause_aux ?(flags=default_flags) hypinfo goal_meta occs clause gl =
  cl_rewrite_clause_aux ~flags hypinfo goal_meta occs clause gl

let cl_rewrite_clause (evm,c) left2right occs clause gl =
  init_setoid ();
  let meta = Evarutil.new_meta() in
  let gl = { gl with sigma = Typeclasses.mark_unresolvables gl.sigma } in
  let env = pf_env gl in
  let evars = Evd.merge (project gl) evm in
  let hypinfo = ref (decompose_setoid_eqhyp env evars c left2right) in
    cl_rewrite_clause_aux hypinfo meta occs clause gl

open Genarg
open Extraargs

let occurrences_of = function
  | n::_ as nl when n < 0 -> (false,List.map abs nl)
  | nl -> 
      if List.exists (fun n -> n < 0) nl then
	error "Illegal negative occurrence number.";
      (true,nl)

TACTIC EXTEND class_rewrite
| [ "clrewrite" orient(o) open_constr(c) "in" hyp(id) "at" occurrences(occ) ] -> [ cl_rewrite_clause c o (occurrences_of occ) (Some (([],id), [])) ]
| [ "clrewrite" orient(o) open_constr(c) "at" occurrences(occ) "in" hyp(id) ] -> [ cl_rewrite_clause c o (occurrences_of occ) (Some (([],id), [])) ]
| [ "clrewrite" orient(o) open_constr(c) "in" hyp(id) ] -> [ cl_rewrite_clause c o all_occurrences (Some (([],id), [])) ]
| [ "clrewrite" orient(o) open_constr(c) "at" occurrences(occ) ] -> [ cl_rewrite_clause c o (occurrences_of occ) None ]
| [ "clrewrite" orient(o) open_constr(c) ] -> [ cl_rewrite_clause c o all_occurrences None ]
END


let clsubstitute o c =
  let is_tac id = match kind_of_term (snd c) with Var id' when id' = id -> true | _ -> false in
    Tacticals.onAllClauses 
      (fun cl -> 
	match cl with
	  | Some ((_,id),_) when is_tac id -> tclIDTAC
	  | _ -> tclTRY (cl_rewrite_clause c o all_occurrences cl))

TACTIC EXTEND substitute
| [ "substitute" orient(o) open_constr(c) ] -> [ clsubstitute o c ]
END

let pr_debug _prc _prlc _prt b =
  if b then Pp.str "debug" else Pp.mt()

ARGUMENT EXTEND debug TYPED AS bool PRINTED BY pr_debug
| [ "debug" ] -> [ true ]
| [ ] -> [ false ]
END

let pr_mode _prc _prlc _prt m =
  match m with
      Some b ->
	if b then Pp.str "depth-first" else Pp.str "breadth-fist" 
    | None -> Pp.mt()
	
ARGUMENT EXTEND search_mode TYPED AS bool option PRINTED BY pr_mode
| [ "dfs" ] -> [ Some true ]
| [ "bfs" ] -> [ Some false ]
| [] -> [ None ]
END

let pr_depth _prc _prlc _prt = function
    Some i -> Util.pr_int i
  | None -> Pp.mt()
	
ARGUMENT EXTEND depth TYPED AS int option PRINTED BY pr_depth
| [ int_or_var_opt(v) ] -> [ match v with Some (ArgArg i) -> Some i | _ -> None ]
END
      
VERNAC COMMAND EXTEND Typeclasses_Settings
 | [ "Typeclasses" "eauto" ":=" debug(d) search_mode(s) depth(depth) ] -> [ 
     let mode = match s with Some t -> t | None -> true in
     let depth = match depth with Some i -> i | None -> default_eauto_depth in
       Typeclasses.solve_instanciations_problem :=
	 solve_inst d mode depth
   ]
END

TACTIC EXTEND typeclasses_eauto
| [ "typeclasses" "eauto" debug(d) search_mode(s) depth(depth) ] -> [ 
    let mode = match s with Some t -> t | None -> true in
    let depth = match depth with Some i -> i | None -> default_eauto_depth in
      fun gl -> 
	let gls = {it = [sig_it gl]; sigma = project gl} in
	let vals v = List.hd v in
	  try typeclasses_eauto d (mode, depth) [] (gls, vals) 
	  with Not_found -> tclFAIL 0 (str" typeclasses eauto failed") gl ]
END


(*     fun gl -> *)
(*     let env = pf_env gl in *)
(*     let sigma = project gl in *)
(*     let proj = sig_it gl in *)
(*     let evd = Evd.create_evar_defs (Evd.add Evd.empty 1 proj) in *)
(*     let mode = match s with Some t -> t | None -> true in *)
(*     let depth = match depth with Some i -> i | None -> default_eauto_depth in *)
(*       match resolve_typeclass_evars d (mode, depth) env evd false with *)
(*       | Some evd' ->  *)
(* 	  let goal = Evd.find (Evd.evars_of evd') 1 in *)
(* 	    (match goal.evar_body with *)
(* 	    | Evar_empty -> tclIDTAC gl *)
(* 	    | Evar_defined b -> refine b gl) *)
(*       | None -> tclIDTAC gl *)
(*   ] *)

let _ = 
  Classes.refine_ref := Refine.refine

(* Compatibility with old Setoids *)
  
TACTIC EXTEND setoid_rewrite
   [ "setoid_rewrite" orient(o) open_constr(c) ]
   -> [ cl_rewrite_clause c o all_occurrences None ]
 | [ "setoid_rewrite" orient(o) open_constr(c) "in" hyp(id) ] ->
      [ cl_rewrite_clause c o all_occurrences (Some (([],id), []))]
 | [ "setoid_rewrite" orient(o) open_constr(c) "at" occurrences(occ) ] ->
      [ cl_rewrite_clause c o (occurrences_of occ) None]
 | [ "setoid_rewrite" orient(o) open_constr(c) "at" occurrences(occ) "in" hyp(id)] ->
      [ cl_rewrite_clause c o (occurrences_of occ) (Some (([],id), []))]
 | [ "setoid_rewrite" orient(o) open_constr(c) "in" hyp(id) "at" occurrences(occ)] ->
      [ cl_rewrite_clause c o (occurrences_of occ) (Some (([],id), []))]
END

(* let solve_obligation lemma =  *)
(*   tclTHEN (Tacinterp.interp (Tacexpr.TacAtom (dummy_loc, Tacexpr.TacAnyConstructor None))) *)
(*     (eapply_with_bindings (Constrintern.interp_constr Evd.empty (Global.env()) lemma, NoBindings)) *)

let mkappc s l = CAppExpl (dummy_loc,(None,(Libnames.Ident (dummy_loc,id_of_string s))),l)

let declare_an_instance n s args =
  ((dummy_loc,Name n), Explicit,
  CAppExpl (dummy_loc, (None, Qualid (dummy_loc, qualid_of_string s)), 
	   args))

let declare_instance a aeq n s = declare_an_instance n s [a;aeq]

let anew_instance binders instance fields = 
  new_instance binders instance (CRecord (dummy_loc,None,fields)) ~generalize:false None

let require_library dirpath =
  let qualid = (dummy_loc, Libnames.qualid_of_dirpath (Libnames.dirpath_of_string dirpath)) in
    Library.require_library [qualid] (Some false)

let declare_instance_refl binders a aeq n lemma = 
  let instance = declare_instance a aeq (add_suffix n "_Reflexive") "Coq.Classes.RelationClasses.Reflexive" 
  in anew_instance binders instance 
       [((dummy_loc,id_of_string "reflexivity"),lemma)]

let declare_instance_sym binders a aeq n lemma = 
  let instance = declare_instance a aeq (add_suffix n "_Symmetric") "Coq.Classes.RelationClasses.Symmetric"
  in anew_instance binders instance 
       [((dummy_loc,id_of_string "symmetry"),lemma)]

let declare_instance_trans binders a aeq n lemma = 
  let instance = declare_instance a aeq (add_suffix n "_Transitive") "Coq.Classes.RelationClasses.Transitive" 
  in anew_instance binders instance 
       [((dummy_loc,id_of_string "transitivity"),lemma)]

let constr_tac = Tacinterp.interp (Tacexpr.TacAtom (dummy_loc, Tacexpr.TacAnyConstructor (false,None)))

let declare_relation ?(binders=[]) a aeq n refl symm trans = 
  init_setoid ();
  let instance = declare_instance a aeq (add_suffix n "_relation") "Coq.Classes.SetoidTactics.SetoidRelation"
  in ignore(anew_instance binders instance []);
  match (refl,symm,trans) with 
      (None, None, None) -> ()
    | (Some lemma1, None, None) -> 
	ignore (declare_instance_refl binders a aeq n lemma1)
    | (None, Some lemma2, None) -> 
	ignore (declare_instance_sym binders a aeq n lemma2)
    | (None, None, Some lemma3) -> 
	ignore (declare_instance_trans binders a aeq n lemma3)
    | (Some lemma1, Some lemma2, None) -> 
	ignore (declare_instance_refl binders a aeq n lemma1); 
	ignore (declare_instance_sym binders a aeq n lemma2)
    | (Some lemma1, None, Some lemma3) -> 
	let _lemma_refl = declare_instance_refl binders a aeq n lemma1 in
	let _lemma_trans = declare_instance_trans binders a aeq n lemma3 in
	let instance = declare_instance a aeq n "Coq.Classes.RelationClasses.PreOrder" 
	in ignore(
	    anew_instance binders instance 
	      [((dummy_loc,id_of_string "PreOrder_Reflexive"), lemma1);
	       ((dummy_loc,id_of_string "PreOrder_Transitive"),lemma3)])
    | (None, Some lemma2, Some lemma3) -> 
	let _lemma_sym = declare_instance_sym binders a aeq n lemma2 in
	let _lemma_trans = declare_instance_trans binders a aeq n lemma3 in
	let instance = declare_instance a aeq n "Coq.Classes.RelationClasses.PER" 
	in ignore(
	    anew_instance binders instance 
	      [((dummy_loc,id_of_string "PER_Symmetric"), lemma2);
	       ((dummy_loc,id_of_string "PER_Transitive"),lemma3)])
     | (Some lemma1, Some lemma2, Some lemma3) -> 
	let _lemma_refl = declare_instance_refl binders a aeq n lemma1 in 
	let _lemma_sym = declare_instance_sym binders a aeq n lemma2 in
	let _lemma_trans = declare_instance_trans binders a aeq n lemma3 in
	let instance = declare_instance a aeq n "Coq.Classes.RelationClasses.Equivalence" 
	in ignore(
	  anew_instance binders instance 
	    [((dummy_loc,id_of_string "Equivalence_Reflexive"), lemma1);
	     ((dummy_loc,id_of_string "Equivalence_Symmetric"), lemma2);
	     ((dummy_loc,id_of_string "Equivalence_Transitive"), lemma3)])

type 'a binders_let_argtype = (local_binder list, 'a) Genarg.abstract_argument_type

let (wit_binders_let : Genarg.tlevel binders_let_argtype),
  (globwit_binders_let : Genarg.glevel binders_let_argtype),
  (rawwit_binders_let : Genarg.rlevel binders_let_argtype) =
  Genarg.create_arg "binders_let"

open Pcoq.Constr

VERNAC COMMAND EXTEND AddRelation
  | [ "Add" "Relation" constr(a) constr(aeq) "reflexivity" "proved" "by" constr(lemma1) 
	"symmetry" "proved" "by" constr(lemma2) "as" ident(n) ] ->
      [ declare_relation a aeq n (Some lemma1) (Some lemma2) None ]

  | [ "Add" "Relation" constr(a) constr(aeq) "reflexivity" "proved" "by" constr(lemma1)  
	"as" ident(n) ] ->
      [ declare_relation a aeq n (Some lemma1) None None ]
  | [ "Add" "Relation" constr(a) constr(aeq)  "as" ident(n) ] -> 
      [ declare_relation a aeq n None None None ]
END

VERNAC COMMAND EXTEND AddRelation2
    [ "Add" "Relation" constr(a) constr(aeq) "symmetry" "proved" "by" constr(lemma2) 
      "as" ident(n) ] ->
      [ declare_relation a aeq n None (Some lemma2) None ]
  | [ "Add" "Relation" constr(a) constr(aeq) "symmetry" "proved" "by" constr(lemma2) "transitivity" "proved" "by" constr(lemma3)  "as" ident(n) ] ->
      [ declare_relation a aeq n None (Some lemma2) (Some lemma3) ]
END

VERNAC COMMAND EXTEND AddRelation3
    [ "Add" "Relation" constr(a) constr(aeq) "reflexivity" "proved" "by" constr(lemma1) 
      "transitivity" "proved" "by" constr(lemma3) "as" ident(n) ] ->
      [ declare_relation a aeq n (Some lemma1) None (Some lemma3) ]
  | [ "Add" "Relation" constr(a) constr(aeq) "reflexivity" "proved" "by" constr(lemma1) 
      "symmetry" "proved" "by" constr(lemma2) "transitivity" "proved" "by" constr(lemma3) 
      "as" ident(n) ] ->
      [ declare_relation a aeq n (Some lemma1) (Some lemma2) (Some lemma3) ] 
  | [ "Add" "Relation" constr(a) constr(aeq) "transitivity" "proved" "by" constr(lemma3)
	"as" ident(n) ] ->  
      [ declare_relation a aeq n None None (Some lemma3) ] 
END

VERNAC COMMAND EXTEND AddParametricRelation
  | [ "Add" "Parametric" "Relation" binders_let(b) ":" constr(a) constr(aeq)
	"reflexivity" "proved" "by" constr(lemma1) 
	"symmetry" "proved" "by" constr(lemma2) "as" ident(n) ] ->
      [ declare_relation ~binders:b a aeq n (Some lemma1) (Some lemma2) None ]
  | [ "Add" "Parametric" "Relation" binders_let(b) ":" constr(a) constr(aeq)
	"reflexivity" "proved" "by" constr(lemma1)  
	"as" ident(n) ] ->
      [ declare_relation ~binders:b a aeq n (Some lemma1) None None ]
  | [ "Add" "Parametric" "Relation" binders_let(b) ":" constr(a) constr(aeq)  "as" ident(n) ] -> 
      [ declare_relation ~binders:b a aeq n None None None ]
END

VERNAC COMMAND EXTEND AddParametricRelation2
    [ "Add" "Parametric" "Relation" binders_let(b) ":" constr(a) constr(aeq) "symmetry" "proved" "by" constr(lemma2) 
      "as" ident(n) ] ->
      [ declare_relation ~binders:b a aeq n None (Some lemma2) None ]
  | [ "Add" "Parametric" "Relation" binders_let(b) ":" constr(a) constr(aeq) "symmetry" "proved" "by" constr(lemma2) "transitivity" "proved" "by" constr(lemma3)  "as" ident(n) ] ->
      [ declare_relation ~binders:b a aeq n None (Some lemma2) (Some lemma3) ]
END

VERNAC COMMAND EXTEND AddParametricRelation3
    [ "Add" "Parametric" "Relation" binders_let(b) ":" constr(a) constr(aeq) "reflexivity" "proved" "by" constr(lemma1) 
      "transitivity" "proved" "by" constr(lemma3) "as" ident(n) ] ->
      [ declare_relation ~binders:b a aeq n (Some lemma1) None (Some lemma3) ]
  | [ "Add" "Parametric" "Relation" binders_let(b) ":" constr(a) constr(aeq) "reflexivity" "proved" "by" constr(lemma1) 
      "symmetry" "proved" "by" constr(lemma2) "transitivity" "proved" "by" constr(lemma3) 
      "as" ident(n) ] ->
      [ declare_relation ~binders:b a aeq n (Some lemma1) (Some lemma2) (Some lemma3) ] 
  | [ "Add" "Parametric" "Relation" binders_let(b) ":" constr(a) constr(aeq) "transitivity" "proved" "by" constr(lemma3)
	"as" ident(n) ] ->  
      [ declare_relation ~binders:b a aeq n None None (Some lemma3) ] 
END

let mk_qualid s =
  Libnames.Qualid (dummy_loc, Libnames.qualid_of_string s)

let cHole = CHole (dummy_loc, None)

open Entries
open Libnames

let respect_projection r ty =
  let ctx, inst = Sign.decompose_prod_assum ty in
  let mor, args = destApp inst in
  let instarg = mkApp (r, rel_vect 0 (List.length ctx)) in
  let app = mkApp (Lazy.force respect_proj, 
		  Array.append args [| instarg |]) in
    it_mkLambda_or_LetIn app ctx
      
let declare_projection n instance_id r =
  let ty = Global.type_of_global r in
  let c = constr_of_global r in
  let term = respect_projection c ty in
  let typ = Typing.type_of (Global.env ()) Evd.empty term in
  let ctx, typ = Sign.decompose_prod_assum typ in
  let typ =
    let n = 
      let rec aux t = 
	match kind_of_term t with
	    App (f, [| a ; a' ; rel; rel' |]) when eq_constr f (Lazy.force respectful) -> 
	      succ (aux rel')
	  | _ -> 0
      in
      let init = 
	match kind_of_term typ with
	    App (f, args) when eq_constr f (Lazy.force respectful) -> 
	      mkApp (f, fst (array_chop (Array.length args - 2) args))
	  | _ -> typ
      in aux init
    in
    let ctx,ccl = Reductionops.decomp_n_prod (Global.env()) Evd.empty (3 * n) typ
    in it_mkProd_or_LetIn ccl ctx 
  in
  let typ = it_mkProd_or_LetIn typ ctx in
  let cst = 
    { const_entry_body = term;
      const_entry_type = Some typ;
      const_entry_opaque = false;
      const_entry_boxed = false }
  in
    ignore(Declare.declare_constant n (Entries.DefinitionEntry cst, Decl_kinds.IsDefinition Decl_kinds.Definition))
          
let build_morphism_signature m =
  let env = Global.env () in
  let m = Constrintern.interp_constr Evd.empty env m in
  let t = Typing.type_of env Evd.empty m in
  let isevars = ref (Evd.create_evar_defs Evd.empty) in
  let cstrs = 
    let rec aux t = 
      match kind_of_term t with
	| Prod (na, a, b) -> 
	    None :: aux b
	| _ -> []
    in aux t
  in
  let t', sig_, evars = build_signature isevars env t cstrs None snd in
  let _ = List.iter
    (fun (ty, rel) -> 
      Option.iter (fun rel ->
	let default = mkApp (Lazy.force default_relation, [| ty; rel |]) in
	  ignore (Evarutil.e_new_evar isevars env default)) 
	rel)
    evars
  in
  let morph = 
    mkApp (Lazy.force morphism_type, [| t; sig_; m |])
  in
  let evd = 
    Typeclasses.resolve_typeclasses ~fail:true ~onlyargs:false env !isevars in
  let m = Evarutil.nf_isevar evd morph in
    Evarutil.check_evars env Evd.empty evd m; m
	
let default_morphism sign m =
  let env = Global.env () in
  let isevars = ref (Evd.create_evar_defs Evd.empty) in
  let t = Typing.type_of env Evd.empty m in
  let _, sign, evars =
    build_signature isevars env t (fst sign) (snd sign) (fun (ty, rel) -> rel)
  in
  let morph =
    mkApp (Lazy.force morphism_type, [| t; sign; m |])
  in
  let mor = resolve_one_typeclass env morph in
    mor, respect_projection mor morph
    	  
let add_setoid binders a aeq t n =
  init_setoid ();
  let _lemma_refl = declare_instance_refl binders a aeq n (mkappc "Seq_refl" [a;aeq;t]) in 
  let _lemma_sym = declare_instance_sym binders a aeq n (mkappc "Seq_sym" [a;aeq;t]) in
  let _lemma_trans = declare_instance_trans binders a aeq n (mkappc "Seq_trans" [a;aeq;t])  in
  let instance = declare_instance a aeq n "Coq.Classes.RelationClasses.Equivalence"
  in ignore(
    anew_instance binders instance
      [((dummy_loc,id_of_string "Equivalence_Reflexive"), mkappc "Seq_refl" [a;aeq;t]);
       ((dummy_loc,id_of_string "Equivalence_Symmetric"), mkappc "Seq_sym" [a;aeq;t]);
       ((dummy_loc,id_of_string "Equivalence_Transitive"), mkappc "Seq_trans" [a;aeq;t])])

let add_morphism_infer m n =
  init_setoid ();
  let instance_id = add_suffix n "_Morphism" in
  let instance = build_morphism_signature m in
    if Lib.is_modtype () then 
      let cst = Declare.declare_internal_constant instance_id
	(Entries.ParameterEntry (instance,false), Decl_kinds.IsAssumption Decl_kinds.Logical)
      in
	add_instance (Typeclasses.new_instance (Lazy.force morphism_class) None false cst);
	declare_projection n instance_id (ConstRef cst)
    else
      let kind = Decl_kinds.Global, Decl_kinds.DefinitionBody Decl_kinds.Instance in
	Flags.silently 
	  (fun () ->
	    Command.start_proof instance_id kind instance 
	      (fun _ -> function
		  Libnames.ConstRef cst -> 
		    add_instance (Typeclasses.new_instance 
				     (Lazy.force morphism_class) None false cst);
		    declare_projection n instance_id (ConstRef cst)
		| _ -> assert false);
	    Pfedit.by (Tacinterp.interp <:tactic< Coq.Classes.SetoidTactics.add_morphism_tactic>>)) ();
	Flags.if_verbose (fun x -> msg (Printer.pr_open_subgoals x)) () 

let add_morphism binders m s n =
  init_setoid ();
  let instance_id = add_suffix n "_Morphism" in
  let instance = 
    ((dummy_loc,Name instance_id), Explicit,
    CAppExpl (dummy_loc, 
	     (None, Qualid (dummy_loc, Libnames.qualid_of_string "Coq.Classes.Morphisms.Morphism")), 
	     [cHole; s; m]))
  in	  
  let tac = Tacinterp.interp <:tactic<add_morphism_tactic>> in
    ignore(new_instance binders instance (CRecord (dummy_loc,None,[]))
	      ~generalize:false ~tac ~hook:(fun cst -> declare_projection n instance_id (ConstRef cst)) None)

VERNAC COMMAND EXTEND AddSetoid1
   [ "Add" "Setoid" constr(a) constr(aeq) constr(t) "as" ident(n) ] ->
     [ add_setoid [] a aeq t n ]
  | [ "Add" "Parametric" "Setoid" binders_let(binders) ":" constr(a) constr(aeq) constr(t) "as" ident(n) ] ->
     [	add_setoid binders a aeq t n ]
  | [ "Add" "Morphism" constr(m) ":" ident(n) ] ->
      [ add_morphism_infer m n ]
  | [ "Add" "Morphism" constr(m) "with" "signature" lconstr(s) "as" ident(n) ] ->
      [ add_morphism [] m s n ]
  | [ "Add" "Parametric" "Morphism" binders_let(binders) ":" constr(m) "with" "signature" lconstr(s) "as" ident(n) ] ->
      [ add_morphism binders m s n ]
END

(** Bind to "rewrite" too *)

(** Taken from original setoid_replace, to emulate the old rewrite semantics where
    lemmas are first instantiated and then rewrite proceeds. *)

let check_evar_map_of_evars_defs evd =
 let metas = Evd.meta_list evd in
 let check_freemetas_is_empty rebus =
  Evd.Metaset.iter
   (fun m ->
     if Evd.meta_defined evd m then () else
      raise
	(Logic.RefinerError (Logic.UnresolvedBindings [Evd.meta_name evd m])))
 in
  List.iter
   (fun (_,binding) ->
     match binding with
        Evd.Cltyp (_,{Evd.rebus=rebus; Evd.freemetas=freemetas}) ->
         check_freemetas_is_empty rebus freemetas
      | Evd.Clval (_,({Evd.rebus=rebus1; Evd.freemetas=freemetas1},_),
                 {Evd.rebus=rebus2; Evd.freemetas=freemetas2}) ->
         check_freemetas_is_empty rebus1 freemetas1 ;
         check_freemetas_is_empty rebus2 freemetas2
   ) metas

let unification_rewrite l2r c1 c2 cl car rel but gl = 
  let env = pf_env gl in
  let (evd',c') =
    try
      (* ~flags:(false,true) to allow to mark occurrences that must not be
         rewritten simply by replacing them with let-defined definitions
         in the context *)
      Unification.w_unify_to_subterm ~flags:rewrite_unif_flags env ((if l2r then c1 else c2),but) cl.evd
    with
	Pretype_errors.PretypeError _ ->
	  (* ~flags:(true,true) to make Ring work (since it really
             exploits conversion) *)
	  Unification.w_unify_to_subterm ~flags:rewrite2_unif_flags
	    env ((if l2r then c1 else c2),but) cl.evd
  in
  let evd' = Typeclasses.resolve_typeclasses ~fail:false env evd' in
  let cl' = {cl with evd = evd'} in
  let cl' =
    let mvs = clenv_dependent false cl' in
      clenv_pose_metas_as_evars cl' mvs
  in
  let nf c = Evarutil.nf_evar (Evd.evars_of cl'.evd) (Clenv.clenv_nf_meta cl' c) in
  let c1 = nf c1 and c2 = nf c2 and car = nf car and rel = nf rel in
  check_evar_map_of_evars_defs cl'.evd;
  let prf = nf (Clenv.clenv_value cl') and prfty = nf (Clenv.clenv_type cl') in
  let cl' = { cl' with templval = mk_freelisted prf ; templtyp = mk_freelisted prfty } in
    {cl=cl'; prf=(mkRel 1); car=car; rel=rel; l2r=l2r; c1=c1; c2=c2; c=None; abs=Some (prf, prfty)}

let get_hyp gl (evm,c) clause l2r = 
  let evars = Evd.merge (project gl) evm in
  let hi = decompose_setoid_eqhyp (pf_env gl) evars c l2r in
  let but = match clause with Some id -> pf_get_hyp_typ gl id | None -> pf_concl gl in
    unification_rewrite hi.l2r hi.c1 hi.c2 hi.cl hi.car hi.rel but gl
	
let general_rewrite_flags = { under_lambdas = false; on_morphisms = false }

let general_s_rewrite cl l2r occs c ~new_goals gl =
  let meta = Evarutil.new_meta() in
  let hypinfo = ref (get_hyp gl c cl l2r) in
  let cl' = Option.map (fun id -> (([],id), [])) cl in
    cl_rewrite_clause_aux ~flags:general_rewrite_flags hypinfo meta occs cl' gl
(*     if fst c = Evd.empty || fst c == project gl then tac gl *)
(*     else *)
(*       let evars = Evd.merge (fst c) (project gl) in *)
(* 	tclTHEN (Refiner.tclEVARS evars) tac gl *)

let general_s_rewrite_clause x =
  init_setoid ();
  match x with
    | None -> general_s_rewrite None
    | Some id -> general_s_rewrite (Some id)
	
let _ = Equality.register_general_setoid_rewrite_clause general_s_rewrite_clause

let is_loaded d =
  let d' = List.map id_of_string d in
  let dir = make_dirpath (List.rev d') in
    Library.library_is_loaded dir

let try_loaded f gl =
  if is_loaded ["Coq";"Classes";"RelationClasses"] then f gl
  else tclFAIL 0 (str"You need to require Coq.Classes.RelationClasses first") gl

let try_classes t gls = 
  try t gls
  with (Pretype_errors.PretypeError _) as e -> raise e

TACTIC EXTEND try_classes
  [ "try_classes" tactic(t) ] -> [ try_classes (snd t) ]
END

open Rawterm
open Environ
open Refiner

let typeclass_app evm gl ?(bindings=NoBindings) c ty =
  let nprod = nb_prod (pf_concl gl) in
  let n = nb_prod ty - nprod in
    if n<0 then error "Apply_tc: theorem has not enough premisses.";
    Refiner.tclTHEN (Refiner.tclEVARS evm)
      (fun gl ->
	let clause = make_clenv_binding_apply gl (Some n) (c,ty) bindings in
	let cl' = evar_clenv_unique_resolver true ~flags:default_unify_flags clause gl in
	let evd' = Typeclasses.resolve_typeclasses cl'.env ~fail:true cl'.evd in
	  tclTHEN (Clenvtac.clenv_refine true {cl' with evd = evd'})
	    tclNORMEVAR gl) gl

open Tacinterp
open Pretyping

let my_ist =
  { lfun = [];
    avoid_ids = [];
    debug = Tactic_debug.DebugOff;
    trace = [] }

let rawconstr_and_expr (evd, c) = c
  
let rawconstr_and_expr_of_rawconstr_bindings = function
  | NoBindings -> NoBindings
  | ImplicitBindings l -> ImplicitBindings (List.map rawconstr_and_expr l)
  | ExplicitBindings l -> ExplicitBindings (List.map (fun (l,b,c) -> (l,b,rawconstr_and_expr c)) l)
      
let my_glob_sign sigma env = {
  ltacvars = [], [] ;
  ltacrecvars = [];
  gsigma = sigma ;
  genv = env }

let typeclass_app_constrexpr t ?(bindings=NoBindings) gl =
  let env = pf_env gl in
  let evars = ref (create_evar_defs (project gl)) in
  let gs = my_glob_sign (project gl) env in
  let t', bl = Tacinterp.intern_constr_with_bindings gs (t,bindings) in
  let j = Pretyping.Default.understand_judgment_tcc evars env (fst t') in
  let bindings = Tacinterp.interp_bindings my_ist gl bl in
    typeclass_app (Evd.evars_of !evars) gl ~bindings:bindings j.uj_val j.uj_type

let typeclass_app_raw (_,t) gl =
  let env = pf_env gl in
  let evars = ref (create_evar_defs (project gl)) in
  let j = Pretyping.Default.understand_judgment_tcc evars env t in
    typeclass_app (Evd.evars_of !evars) gl j.uj_val j.uj_type

let pr_gen prc _prlc _prtac c = prc c

let pr_ceb _prc _prlc _prtac raw = mt ()

let interp_constr_expr_bindings _ _ t = t

let intern_constr_expr_bindings ist t = t

open Pcoq.Tactic

type constr_expr_bindings = constr_expr with_bindings

ARGUMENT EXTEND constr_expr_bindings
    TYPED AS constr_expr_bindings
    PRINTED BY pr_ceb
     
     INTERPRETED BY interp_constr_expr_bindings	
    GLOBALIZED BY intern_constr_expr_bindings

     
  [ constr_with_bindings(c) ] -> [ c ]
END

TACTIC EXTEND apply_typeclasses
[ "typeclass_app" constr_expr_bindings(t) ] -> [ typeclass_app_constrexpr (fst t) ~bindings:(snd t) ]
END
TACTIC EXTEND apply_typeclasses_abbrev
[ "tcapp" raw(t) ] -> [ typeclass_app_raw t ]
END

(* [setoid_]{reflexivity,symmetry,transitivity} tactics *)

let not_declared env ty rel =
  tclFAIL 0 (str" The relation " ++ Printer.pr_constr_env env rel ++ str" is not a declared " ++ 
		str ty ++ str" relation. Maybe you need to require the Setoid library")

let relation_of_constr env c = 
  match kind_of_term c with
    | App (f, args) when Array.length args >= 2 -> 
	let relargs, args = array_chop (Array.length args - 2) args in
	  mkApp (f, relargs), args
    | _ -> errorlabstrm "relation_of_constr" 
	(str "The term " ++ Printer.pr_constr_env env c ++ str" is not an applied relation.")
	
let setoid_proof gl ty fn fallback =
  let env = pf_env gl in
    try 
      let rel, args = relation_of_constr env (pf_concl gl) in
      let evm, car = project gl, pf_type_of gl args.(0) in
	fn env evm car rel gl
    with e -> 
      match fallback gl with
      | Some tac -> tac gl
      | None -> 
	  match e with
	  | Not_found ->
	      let rel, args = relation_of_constr env (pf_concl gl) in
		not_declared env ty rel gl
	  | _ -> raise e
	      
let setoid_reflexivity gl =
  setoid_proof gl "reflexive" 
    (fun env evm car rel -> apply (get_reflexive_proof env evm car rel))
    (reflexivity_red true)
	  
let setoid_symmetry gl =
  setoid_proof gl "symmetric" 
    (fun env evm car rel -> apply (get_symmetric_proof env evm car rel))
    (symmetry_red true)
    
let setoid_transitivity c gl =
  setoid_proof gl "transitive" 
    (fun env evm car rel ->
      apply_with_bindings
	((get_transitive_proof env evm car rel),
	Rawterm.ExplicitBindings [ dummy_loc, Rawterm.NamedHyp (id_of_string "y"), c ]))
    (transitivity_red true c)
    
(*
  let setoid_proof gl ty ?(bindings=NoBindings) meth fallback =
  try
  typeclass_app_constrexpr 
  (CRef (Qualid (dummy_loc, Nametab.shortest_qualid_of_global Idset.empty
  (Lazy.force meth)))) ~bindings gl
  with Not_found | Typeclasses_errors.TypeClassError (_, _) |
      Stdpp.Exc_located (_, Typeclasses_errors.TypeClassError (_, _)) -> 
	match fallback gl with 
	| Some tac -> tac gl
	| None ->
	    let env = pf_env gl in
	    let rel, args = relation_of_constr env (pf_concl gl) in
	      not_declared env ty rel gl
      
let setoid_reflexivity gl =
  setoid_proof gl "reflexive" reflexive_proof_global (reflexivity_red true)
    
let setoid_symmetry gl =
  setoid_proof gl "symmetric" symmetric_proof_global (symmetry_red true)
    
let setoid_transitivity c gl =
  let binding_name = 
    next_ident_away (id_of_string "y") (ids_of_named_context (named_context (pf_env gl))) 
  in
    setoid_proof gl "transitive" 
      ~bindings:(Rawterm.ExplicitBindings [ dummy_loc, Rawterm.NamedHyp binding_name, constrIn c ])
      transitive_proof_global (transitivity_red true c)
*)
let setoid_symmetry_in id gl =
  let ctype = pf_type_of gl (mkVar id) in
  let binders,concl = Sign.decompose_prod_assum ctype in
  let (equiv, args) = decompose_app concl in
  let rec split_last_two = function
    | [c1;c2] -> [],(c1, c2)
    | x::y::z -> let l,res = split_last_two (y::z) in x::l, res
    | _ -> error "The term provided is not an equivalence."
  in
  let others,(c1,c2) = split_last_two args in
  let he,c1,c2 =  mkApp (equiv, Array.of_list others),c1,c2 in
  let new_hyp' =  mkApp (he, [| c2 ; c1 |]) in
  let new_hyp = it_mkProd_or_LetIn new_hyp'  binders in
    tclTHENS (cut new_hyp)
      [ intro_replacing id;
	tclTHENLIST [ intros; setoid_symmetry; apply (mkVar id); Tactics.assumption ] ]
      gl

let _ = Tactics.register_setoid_reflexivity setoid_reflexivity
let _ = Tactics.register_setoid_symmetry setoid_symmetry
let _ = Tactics.register_setoid_symmetry_in setoid_symmetry_in
let _ = Tactics.register_setoid_transitivity setoid_transitivity

TACTIC EXTEND setoid_symmetry
   [ "setoid_symmetry" ] -> [ setoid_symmetry ]
 | [ "setoid_symmetry" "in" hyp(n) ] -> [ setoid_symmetry_in n ]
END

TACTIC EXTEND setoid_reflexivity
[ "setoid_reflexivity" ] -> [ setoid_reflexivity ]
END

TACTIC EXTEND setoid_transitivity
[ "setoid_transitivity" constr(t) ] -> [ setoid_transitivity t ]
END

let rec head_of_constr t =
  let t = strip_outer_cast(collapse_appl t) in
    match kind_of_term t with
    | Prod (_,_,c2)  -> head_of_constr c2 
    | LetIn (_,_,_,c2) -> head_of_constr c2
    | App (f,args)  -> head_of_constr f
    | _      -> t
      
TACTIC EXTEND head_of_constr
  [ "head_of_constr" ident(h) constr(c) ] -> [
    let c = head_of_constr c in
      letin_tac None (Name h) c None allHyps
  ]
END


let coq_List_nth =  lazy (gen_constant ["Lists"; "List"] "nth")
let coq_List_cons =  lazy (gen_constant ["Lists"; "List"] "cons")
let coq_List_nil =  lazy (gen_constant ["Lists"; "List"] "nil")

let freevars c =
  let rec frec acc c = match kind_of_term c with
    | Var id       -> Idset.add id acc
    | _ -> fold_constr frec acc c
  in 
  frec Idset.empty c

let coq_zero =  lazy (gen_constant ["Init"; "Datatypes"] "O")
let coq_succ =  lazy (gen_constant ["Init"; "Datatypes"] "S")
let coq_nat =  lazy (gen_constant ["Init"; "Datatypes"] "nat")

let rec coq_nat_of_int = function
  | 0 -> Lazy.force coq_zero
  | n -> mkApp (Lazy.force coq_succ, [| coq_nat_of_int (pred n) |])

let varify_constr_list ty def varh c =
  let vars = Idset.elements (freevars c) in
  let mkaccess i = 
    mkApp (Lazy.force coq_List_nth,
	  [| ty; coq_nat_of_int i; varh; def |])
  in
  let l = List.fold_right (fun id acc -> 
    mkApp (Lazy.force coq_List_cons, [| ty ; mkVar id; acc |]))
    vars (mkApp (Lazy.force coq_List_nil, [| ty |]))
  in
  let subst = 
    list_map_i (fun i id -> (id, mkaccess i)) 0 vars
  in
    l, replace_vars subst c

let coq_varmap_empty =  lazy (gen_constant ["ring"; "Quote"] "Empty_vm")
let coq_varmap_node =  lazy (gen_constant ["ring"; "Quote"] "Node_vm")
(*  | Node_vm : A -> varmap -> varmap -> varmap. *)
  
let coq_varmap_lookup =  lazy (gen_constant ["ring"; "Quote"] "varmap_find")

let coq_index_left =  lazy (gen_constant ["ring"; "Quote"] "Left_idx")
let coq_index_right =  lazy (gen_constant ["ring"; "Quote"] "Right_idx")
let coq_index_end =  lazy (gen_constant ["ring"; "Quote"] "End_idx")

let rec split_interleaved l r = function
  | hd :: hd' :: tl' ->
      split_interleaved (hd :: l) (hd' :: r) tl'
  | hd :: [] -> (List.rev (hd :: l), List.rev r)
  | [] -> (List.rev l, List.rev r)
      
(* let rec mkidx i acc = *)
(*   if i mod 2 = 0 then *)
(*     let acc' = mkApp (Lazy.force coq_index_left, [|acc|]) in *)
(*       if i = 0 then acc' *)
(*       else mkidx (i / 2) acc' *)
(*   else *)
(*     let acc' = mkApp (Lazy.force coq_index_right, [|acc|]) in *)
(*       if i = 1 then acc' *)
(*       else mkidx (i / 2) acc' *)

let rec mkidx i p =
  if i mod 2 = 0 then
    if i = 0 then mkApp (Lazy.force coq_index_left, [|Lazy.force coq_index_end|])
    else mkApp (Lazy.force coq_index_left, [|mkidx (i - p) (2 * p)|])
  else if i = 1 then mkApp (Lazy.force coq_index_right, [|Lazy.force coq_index_end|])
  else mkApp (Lazy.force coq_index_right, [|mkidx (i - p) (2 * p)|])
	
let varify_constr_varmap ty def varh c =
  let vars = Idset.elements (freevars c) in
  let mkaccess i = 
    mkApp (Lazy.force coq_varmap_lookup,
	  [| ty; def; i; varh |])
  in
  let rec vmap_aux l cont = 
    match l with 
    | [] -> [], mkApp (Lazy.force coq_varmap_empty, [| ty |])
    | hd :: tl -> 
	let left, right = split_interleaved [] [] tl in
	let leftvars, leftmap = vmap_aux left (fun x -> cont (mkApp (Lazy.force coq_index_left, [| x |]))) in
	let rightvars, rightmap = vmap_aux right (fun x -> cont (mkApp (Lazy.force coq_index_right, [| x |]))) in
	  (hd, cont (Lazy.force coq_index_end)) :: leftvars @ rightvars, 
	mkApp (Lazy.force coq_varmap_node, [| ty; hd; leftmap ; rightmap |])
  in
  let subst, vmap = vmap_aux (def :: List.map (fun x -> mkVar x) vars) (fun x -> x) in
  let subst = List.map (fun (id, x) -> (destVar id, mkaccess x)) (List.tl subst) in
    vmap, replace_vars subst c
  

TACTIC EXTEND varify
  [ "varify" ident(varh) ident(h') constr(ty) constr(def) constr(c) ] -> [
    let vars, c' = varify_constr_varmap ty def (mkVar varh) c in
      tclTHEN (letin_tac None (Name varh) vars None allHyps)
	(letin_tac None (Name h') c' None allHyps)
  ]
END


