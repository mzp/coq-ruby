(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, * CNRS-Ecole Polytechnique-INRIA Futurs-Universite Paris Sud *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(* $Id: Bool_nat.v 5920 2004-07-16 20:01:26Z herbelin $ *)

Require Export Compare_dec.
Require Export Peano_dec.
Require Import Sumbool.

Open Local Scope nat_scope.

Implicit Types m n x y : nat.

(** The decidability of equality and order relations over
    type [nat] give some boolean functions with the adequate specification. *)

Definition notzerop n := sumbool_not _ _ (zerop n).
Definition lt_ge_dec : forall x y, {x < y} + {x >= y} :=
  fun n m => sumbool_not _ _ (le_lt_dec m n).

Definition nat_lt_ge_bool x y := bool_of_sumbool (lt_ge_dec x y).
Definition nat_ge_lt_bool x y :=
  bool_of_sumbool (sumbool_not _ _ (lt_ge_dec x y)).

Definition nat_le_gt_bool x y := bool_of_sumbool (le_gt_dec x y).
Definition nat_gt_le_bool x y :=
  bool_of_sumbool (sumbool_not _ _ (le_gt_dec x y)).

Definition nat_eq_bool x y := bool_of_sumbool (eq_nat_dec x y).
Definition nat_noteq_bool x y :=
  bool_of_sumbool (sumbool_not _ _ (eq_nat_dec x y)).

Definition zerop_bool x := bool_of_sumbool (zerop x).
Definition notzerop_bool x := bool_of_sumbool (notzerop x).