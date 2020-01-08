(* ML supports mutability but not in variables,
not in lists, not in tuples... if you want
to have mutable data, you need a separate construct:

MUTABLE REFERENCES

t ref (* where t is type. E.g. int ref *)
*)

(* Expressions *)

ref e       (*To create a reference with initial content e*)
e1 := e2    (* To update the contents *)
!e          (* to retrieve contents*)


val x = ref 42
val y = ref 42
val z = x
val _ = x := 43
val w = (!y) + (!z) (* 42 + 43 = 85 *)
(* x + 1 does not type-check *)
