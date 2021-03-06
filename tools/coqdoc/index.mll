(* -*- compile-command: "make -C ../.. bin/coqdoc" -*- *)
(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, * CNRS-Ecole Polytechnique-INRIA Futurs-Universite Paris Sud *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*i $Id: index.mll 11790 2009-01-15 20:19:58Z msozeau $ i*)

{

open Filename
open Lexing 
open Printf

open Cdglobals

type loc = int

type entry_type = 
  | Library
  | Module
  | Definition
  | Inductive
  | Constructor
  | Lemma
  | Record
  | Projection
  | Instance
  | Class
  | Method
  | Variable
  | Axiom
  | TacticDefinition
  | Abbreviation
  | Notation
  | Section

type index_entry = 
  | Def of string * entry_type
  | Ref of coq_module * string * entry_type
  | Mod of coq_module * string

let current_type = ref Library
let current_library = ref ""
  (** refers to the file being parsed *)

(** [deftable] stores only definitions and is used to interpolate idents 
    inside comments, which are not globalized otherwise. *)

let deftable = Hashtbl.create 97

(** [reftable] stores references and definitions *)
let reftable = Hashtbl.create 97
  
let full_ident sp id =
  if sp <> "<>" then 
    if id <> "<>" then 
      sp ^ "." ^ id 
    else sp 
  else if id <> "<>" 
  then id 
  else ""
    
let add_def loc ty sp id = 
  Hashtbl.add reftable (!current_library, loc) (Def (full_ident sp id, ty));
  Hashtbl.add deftable id (Ref (!current_library, full_ident sp id, ty))
    
let add_ref m loc m' sp id ty = 
  if Hashtbl.mem reftable (m, loc) then ()
  else Hashtbl.add reftable (m, loc) (Ref (m', full_ident sp id, ty));
  let idx = if id = "<>" then m' else id in
    if Hashtbl.mem deftable idx then ()
    else Hashtbl.add deftable idx (Ref (m', full_ident sp id, ty))  
    
let add_mod m loc m' id = 
  Hashtbl.add reftable (m, loc) (Mod (m', id));
  Hashtbl.add deftable m (Mod (m', id))
    
let find m l = Hashtbl.find reftable (m, l)
  
let find_string m s = Hashtbl.find deftable s
  
(*s Manipulating path prefixes *) 

type stack = string list 

let rec string_of_stack st =
  match st with
    | [] -> ""
    | x::[] -> x
    | x::tl -> (string_of_stack tl) ^ "." ^ x

let empty_stack = []

let module_stack = ref empty_stack
let section_stack = ref empty_stack

let init_stack () =
  module_stack := empty_stack; section_stack := empty_stack

let push st p = st := p::!st
let pop st = 
  match !st with 
    | [] -> ()
    | _::tl -> st := tl
	
let head st =
  match st with
    | [] -> ""
    | x::_ -> x

let begin_module m = push module_stack m
let begin_section s = push section_stack s

let end_block id =
  (** determines if it ends a module or a section and pops the stack *)
  if ((String.compare (head !module_stack) id ) == 0) then
    pop module_stack
  else if ((String.compare (head !section_stack) id) == 0) then
    pop section_stack
  else
    ()

let make_fullid id = 
  (** prepends the current module path to an id *)
  let path = string_of_stack !module_stack in
    if String.length path > 0 then
      path ^ "." ^ id
    else 
      id


(* Coq modules *)

let split_sp s = 
  try
    let i = String.rindex s '.' in
      String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)
  with 
      Not_found -> "", s

let modules = Hashtbl.create 97
let local_modules = Hashtbl.create 97

let add_module m =
  let _,id = split_sp m in
  Hashtbl.add modules id m;
  Hashtbl.add local_modules m ()

type module_kind = Local | Coqlib | Unknown

let coq_module m = String.length m >= 4 && String.sub m 0 4 = "Coq."

let find_module m =
  if Hashtbl.mem local_modules m then 
    Local
  else if coq_module m then
    Coqlib
  else
    Unknown


(* Building indexes *)

type 'a index = { 
  idx_name : string;
  idx_entries : (char * (string * 'a) list) list;
  idx_size : int }
		  
let map f i = 
  { i with idx_entries = 
      List.map 
	(fun (c,l) -> (c, List.map (fun (s,x) -> (s,f s x)) l)) 
	i.idx_entries }

let compare_entries (s1,_) (s2,_) = Alpha.compare_string s1 s2

let sort_entries el =
  let t = Hashtbl.create 97 in
    List.iter 
      (fun c -> Hashtbl.add t c [])
      ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 
       'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'; '_'];  
    List.iter 
      (fun ((s,_) as e) -> 
	 let c = Alpha.norm_char s.[0] in 
	 let l = try Hashtbl.find t c with Not_found -> [] in
	   Hashtbl.replace t c (e :: l)) 
      el;
    let res = ref [] in
      Hashtbl.iter 
	(fun c l -> res := (c, List.sort compare_entries l) :: !res) t;
      List.sort (fun (c1,_) (c2,_) -> Alpha.compare_char c1 c2) !res
	
let index_size = List.fold_left (fun s (_,l) -> s + List.length l) 0
  
let hashtbl_elements h = Hashtbl.fold (fun x y l -> (x,y)::l) h []
  
let type_name = function
  | Library -> "library"
  | Module -> "module"
  | Definition -> "definition"
  | Inductive -> "inductive"
  | Constructor -> "constructor"
  | Lemma -> "lemma"
  | Record -> "record"
  | Projection -> "projection"
  | Instance -> "instance"
  | Class -> "class"
  | Method -> "method"
  | Variable -> "variable"
  | Axiom -> "axiom"
  | TacticDefinition -> "tactic"
  | Abbreviation -> "abbreviation"
  | Notation -> "notation"
  | Section -> "section"

let all_entries () =
  let gl = ref [] in
  let add_g s m t = gl := (s,(m,t)) :: !gl in
  let bt = Hashtbl.create 11 in
  let add_bt t s m =
    let l = try Hashtbl.find bt t with Not_found -> [] in
      Hashtbl.replace bt t ((s,m) :: l)
  in
  let classify (m,_) e = match e with 
    | Def (s,t) -> add_g s m t; add_bt t s m
    | Ref _ | Mod _ -> ()
  in
    Hashtbl.iter classify reftable;
    Hashtbl.iter (fun id m -> add_g id m Library; add_bt Library id m) modules;
    { idx_name = "global"; 
      idx_entries = sort_entries !gl; 
      idx_size = List.length !gl },
    Hashtbl.fold (fun t e l -> (t, { idx_name = type_name t; 
				   idx_entries = sort_entries e; 
				   idx_size = List.length e }) :: l) bt []
    
}

(*s Shortcuts for regular expressions. *)
let digit = ['0'-'9']
let num = digit+

let space = 
  [' ' '\010' '\013' '\009' '\012']
let firstchar = 
  ['$' 'A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255']
let identchar = 
  ['$' 'A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' 
     '\'' '0'-'9']
let id = firstchar identchar*
let pfx_id = (id '.')*
let ident = id | pfx_id id

let begin_hide = "(*" space* "begin" space+ "hide" space* "*)"
let end_hide = "(*" space* "end" space+ "hide" space* "*)"
  
(*s Indexing entry point. *)
  
rule traverse = parse
  | ("Program" space+)? "Definition" space
      { current_type := Definition; index_ident lexbuf; traverse lexbuf }
  | "Tactic" space+ "Definition" space
      { current_type := TacticDefinition; index_ident lexbuf; traverse lexbuf }
  | ("Axiom" | "Parameter") space 
      { current_type := Axiom; index_ident lexbuf; traverse lexbuf }
  | ("Program" space+)? "Fixpoint" space
      { current_type := Definition; index_ident lexbuf; fixpoint lexbuf;
	traverse lexbuf }
  | ("Program" space+)? ("Lemma" | "Theorem") space
      { current_type := Lemma; index_ident lexbuf; traverse lexbuf }
  | "Obligation" space num ("of" ident)?
      { current_type := Lemma; index_ident lexbuf; traverse lexbuf }
  | "Inductive" space
      { current_type := Inductive; 
	index_ident lexbuf; inductive lexbuf; traverse lexbuf }
  | "Record" space
      { current_type := Inductive; index_ident lexbuf; traverse lexbuf }
  | "Module" (space+ "Type")? space
      { current_type := Module; module_ident lexbuf; traverse lexbuf }
(*i***
  | "Variable" 's'? space
      { current_type := Variable; index_idents lexbuf; traverse lexbuf }
***i*)
  | "Require" (space+ ("Export"|"Import"))? 
      { module_refs lexbuf; traverse lexbuf }
  | "End" space+ 
      { end_ident lexbuf; traverse lexbuf }
  | begin_hide 
      { skip_hide lexbuf; traverse lexbuf }
  | "(*" 
      { comment lexbuf; traverse lexbuf }
  | '"'
      { string lexbuf; traverse lexbuf }
  | eof          
      { () }
  | _            
      { traverse lexbuf }

(*s Index one identifier. *)

and index_ident = parse
  | space+ 
      { index_ident lexbuf }
  | ident  
      { let fullid = 
	  let id = lexeme lexbuf in
	    match !current_type with
	      | Definition
	      | Inductive
	      | Constructor 
	      | Lemma -> make_fullid id
	      | _ -> id 
	in 
	  add_def (lexeme_start lexbuf) !current_type "" fullid }
  | eof    
      { () }
  | _      
      { () }

(*s Index identifiers separated by blanks and/or commas. *)

and index_idents = parse
  | space+ | ','
      { index_idents lexbuf }
  | ident  
      { add_def (lexeme_start lexbuf) !current_type "" (lexeme lexbuf);
	index_idents lexbuf }
  | eof    
      { () }
  | _
      { skip_until_point lexbuf }
      
(*s Index identifiers in an inductive definition (types and constructors). *)
      
and inductive = parse
  | '|' | ":=" space* '|'? 
	{ current_type := Constructor; index_ident lexbuf; inductive lexbuf }
  | "with" space
      { current_type := Inductive; index_ident lexbuf; inductive lexbuf }
  | '.'    
      { () }
  | eof    
      { () }
  | _      
      { inductive lexbuf }
      
(*s Index identifiers in a Fixpoint declaration. *)
      
and fixpoint = parse
  | "with" space
      { index_ident lexbuf; fixpoint lexbuf }
  | '.' 
      { () }
  | eof    
      { () }
  | _      
      { fixpoint lexbuf }
      
(*s Skip a possibly nested comment. *)
      
and comment = parse
  | "*)" { () }
  | "(*" { comment lexbuf; comment lexbuf }
  | '"'  { string lexbuf; comment lexbuf }
  | eof  { eprintf " *** Unterminated comment while indexing" }
  | _    { comment lexbuf }

(*s Skip a constant string. *)
      
and string = parse
  | '"'  { () }
  | eof  { eprintf " *** Unterminated string while indexing" }
  | _    { string lexbuf }

(*s Skip everything until the next dot. *)
      
and skip_until_point = parse
  | '.'  { () }
  | eof  { () }
  | _    { skip_until_point lexbuf }
      
(*s Skip everything until [(* end hide *)] *)

and skip_hide = parse
  | eof | end_hide { () }
  | _ { skip_hide lexbuf }

and end_ident = parse
  | space+ 
      { end_ident lexbuf }
  | ident  
      { let id = lexeme lexbuf in end_block id }
  | eof    
      { () }
  | _      
      { () }

and module_ident = parse
  | space+
      { module_ident lexbuf }
  | '"' { string lexbuf; module_ident lexbuf }
  | ident space* ":="
      { () }
  | ident
      { let id = lexeme lexbuf in
	  begin_module id; add_def (lexeme_start lexbuf) !current_type "" id }
  | eof
      { () }
  | _
      { () }

(*s parse module names *)

and module_refs = parse
  | space+ 
      { module_refs lexbuf }
  | ident  
      { let id =  lexeme lexbuf in
	  (try
	     add_mod !current_library (lexeme_start lexbuf) (Hashtbl.find modules id) id
	   with
	       Not_found -> ()
	  ); 
	module_refs lexbuf }
  | eof    
      { () }
  | _      
      { () }

{
  let type_of_string = function
    | "def" | "coe" | "subclass" | "canonstruc" | "fix" | "cofix"
    | "ex" | "scheme" -> Definition
    | "prf" | "thm" -> Lemma
    | "ind" | "coind" -> Inductive
    | "constr" -> Constructor
    | "rec" | "corec" -> Record
    | "proj" -> Projection
    | "class" -> Class
    | "meth" -> Method
    | "inst" -> Instance
    | "var" -> Variable
    | "defax" | "prfax" | "ax" -> Axiom
    | "syndef" -> Abbreviation
    | "not" -> Notation
    | "lib" -> Library
    | "mod" | "modtype" -> Module
    | "tac" -> TacticDefinition
    | "sec" -> Section
    | s -> raise (Invalid_argument ("type_of_string:" ^ s))
	
  let read_glob f = 
    let c = open_in f in
    let cur_mod = ref "" in
    try
      while true do
	let s = input_line c in
	let n = String.length s in
	if n > 0 then begin
	  match s.[0] with
	    | 'F' -> 
		cur_mod := String.sub s 1 (n - 1);
		current_library := !cur_mod
	    | 'R' ->
		(try
		    Scanf.sscanf s "R%d %s %s %s %s"
		      (fun loc lib_dp sp id ty ->
			add_ref !cur_mod loc lib_dp sp id (type_of_string ty))
		  with _ -> ())
	    | _ -> 
		try Scanf.sscanf s "%s %d %s %s"
		  (fun ty loc sp id -> add_def loc (type_of_string ty) sp id)
		with Scanf.Scan_failure _ -> ()
	end
      done; assert false
    with End_of_file -> 
      close_in c; !cur_mod
	
  let scan_file f m = 
    init_stack (); current_library := m;
    let c = open_in f in
    let lb = from_channel c in
      traverse lb;
      close_in c
}
