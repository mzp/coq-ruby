Definition id {A} (x : A) := x.
Definition const {A} (x y : A) := x.
Definition app {A B C} (x : A -> B -> C) (y : A) (z : B) :=
  let f := x y in
    f z.

Extraction Language Ruby.
Extraction "test" id const app.
