(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, * CNRS-Ecole Polytechnique-INRIA Futurs-Universite Paris Sud *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(* Certification of Imperative Programs / Jean-Christophe Filli�tre *)

(* $Id: Correctness.v 5920 2004-07-16 20:01:26Z herbelin $ *)

(* Correctness is base on the tactic Refine (developped on purpose) *)

Require Export Tuples.

Require Export ProgInt.
Require Export ProgBool.
Require Export Zwf.

Require Export Arrays.

(*
Token "'".
*)