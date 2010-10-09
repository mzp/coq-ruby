(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, * CNRS-Ecole Polytechnique-INRIA Futurs-Universite Paris Sud *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(* $Id: Rpow_def.v 10923 2008-05-12 18:25:06Z herbelin $ *)

Require Import Rdefinitions.

Fixpoint pow (r:R) (n:nat) {struct n} : R :=
  match n with
    | O => R1
    | S n => Rmult r (pow r n)
  end.
