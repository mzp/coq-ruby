(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, * CNRS-Ecole Polytechnique-INRIA Futurs-Universite Paris Sud *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(* $Id: Fourier.v 11672 2008-12-12 14:45:09Z herbelin $ *)

(* "Fourier's method to solve linear inequations/equations systems.".*)

Require Export Fourier_util.
Require Export LegacyField.
Require Export DiscrR.

Ltac fourier := abstract (fourierz; field; discrR).

Ltac fourier_eq := apply Rge_antisym; fourier.
