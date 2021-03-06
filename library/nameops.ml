(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, * CNRS-Ecole Polytechnique-INRIA Futurs-Universite Paris Sud *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(* $Id: nameops.ml 9433 2006-12-12 09:38:53Z herbelin $ *)

open Pp
open Util
open Names

(* Identifiers *)

let pr_id id = str (string_of_id id)

let pr_name = function
  | Anonymous -> str "_"
  | Name id -> pr_id id

(* Utilities *)

let code_of_0 = Char.code '0'
let code_of_9 = Char.code '9'

let cut_ident skip_quote s =
  let s = string_of_id s in
  let slen = String.length s in
  (* [n'] is the position of the first non nullary digit *)
  let rec numpart n n' =
    if n = 0 then 
      (* ident made of _ and digits only [and ' if skip_quote]: don't cut it *)
      slen
    else 
      let c = Char.code (String.get s (n-1)) in
      if c = code_of_0 && n <> slen then 
	numpart (n-1) n' 
      else if code_of_0 <= c && c <= code_of_9 then 
	numpart (n-1) (n-1)
      else if skip_quote & (c = Char.code '\'' || c = Char.code '_') then
	numpart (n-1) (n-1)
      else
	n'
  in
  numpart slen slen

let repr_ident s =
  let numstart = cut_ident false s in
  let s = string_of_id s in
  let slen = String.length s in
  if numstart = slen then 
    (s, None)
  else
    (String.sub s 0 numstart,
     Some (int_of_string (String.sub s numstart (slen - numstart))))

let make_ident sa = function
  | Some n -> 
      let c = Char.code (String.get sa (String.length sa -1)) in
      let s =
        if c < code_of_0 or c > code_of_9 then sa ^ (string_of_int n)
        else sa ^ "_" ^ (string_of_int n) in
      id_of_string s
  | None -> id_of_string (String.copy sa)

let root_of_id id =
  let suffixstart = cut_ident true id in
  id_of_string (String.sub (string_of_id id) 0 suffixstart)

(* Rem: semantics is a bit different, if an ident starts with toto00 then
  after successive renamings it comes to toto09, then it goes on with toto10 *)
let lift_subscript id =
  let id = string_of_id id in
  let len = String.length id in
  let rec add carrypos =
    let c = id.[carrypos] in
    if is_digit c then
      if c = '9' then begin
	assert (carrypos>0);
	add (carrypos-1)
      end
      else begin
	let newid = String.copy id in
	String.fill newid (carrypos+1) (len-1-carrypos) '0';
	newid.[carrypos] <- Char.chr (Char.code c + 1);
	newid
      end
    else begin
      let newid = id^"0" in
      if carrypos < len-1 then begin
	String.fill newid (carrypos+1) (len-1-carrypos) '0';
	newid.[carrypos+1] <- '1'
      end;
      newid
    end
  in id_of_string (add (len-1))

let has_subscript id =
  let id = string_of_id id in
  is_digit (id.[String.length id - 1])

let forget_subscript id =
  let numstart = cut_ident false id in
  let newid = String.make (numstart+1) '0' in
  String.blit (string_of_id id) 0 newid 0 numstart;
  (id_of_string newid)

let add_suffix id s = id_of_string (string_of_id id ^ s)
let add_prefix s id = id_of_string (s ^ string_of_id id)

let atompart_of_id id = fst (repr_ident id)

(* Fresh names *)

let lift_ident = lift_subscript

let next_ident_away id avoid = 
  if List.mem id avoid then
    let id0 = if not (has_subscript id) then id else 
    (* Ce serait sans doute mieux avec quelque chose inspir� de 
       *** make_ident id (Some 0) *** mais �a brise la compatibilit�... *)
    forget_subscript id in
    let rec name_rec id =
      if List.mem id avoid then name_rec (lift_ident id) else id in 
    name_rec id0
  else id

let next_ident_away_from id avoid = 
  let rec name_rec id =
    if List.mem id avoid then name_rec (lift_ident id) else id in 
  name_rec id 

(* Names *)

let out_name = function
  | Name id -> id
  | Anonymous -> failwith "out_name: expects a defined name"

let name_fold f na a =
  match na with
  | Name id -> f id a
  | Anonymous -> a

let name_cons na l =
  match na with
  | Anonymous -> l 
  | Name id -> id::l

let name_app f = function
  | Name id -> Name (f id)
  | Anonymous -> Anonymous

let name_fold_map f e = function
  | Name id -> let (e,id) = f e id in (e,Name id)
  | Anonymous -> e,Anonymous

let next_name_away_with_default default name l = 
  match name with
    | Name str  -> next_ident_away str l
    | Anonymous -> next_ident_away (id_of_string default) l

let next_name_away = next_name_away_with_default "H"

let pr_lab l = str (string_of_label l)

let default_library = Names.initial_dir (* = ["Top"] *)

(*s Roots of the space of absolute names *)
let coq_root = id_of_string "Coq"
let default_root_prefix = make_dirpath []

(* Metavariables *)
let pr_meta = Pp.int
let string_of_meta = string_of_int
