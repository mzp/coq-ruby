Definition id {A} (x : A) := x.
Definition const {A} (x y : A) := x.
Definition app {A B C} (x : A -> B -> C) (y : A) (z : B) :=
  let f := x y in
    f z.

Definition one := 1.
Definition is_zero (n : nat) :=
  match n with
    | O => true
    | S _ => false
  end.

Definition fact1 := id ((fix f n :=
match n with
| O => 1
| S m => n * f m
end)).

Definition three := 3.
Extraction Language Ruby.
Extraction "test" fact1 three.
