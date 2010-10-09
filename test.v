Fixpoint fact n :=
match n with
  O => 1
| S O => 1
| S m => n * fact m
end.

CoInductive Stream (A: Type) : Type :=
  | Cons : A -> Stream A -> Stream A.
Definition head {A} (s : Stream A) :=
  match s with
    | Cons a _ => a
  end.

CoFixpoint repeat {A} (a:A) : Stream A :=
  Cons A a (repeat a).

Extraction Language Ruby.
Extraction "stream" fact repeat head.
