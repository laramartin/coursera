datatype exp = 
  Constant of int
  | Negate of exp
  | Add of exp * exp
  | Multiply of exp * exp

fun eval (Constant i) = i
  | eval (Add(e1,e2)) = (eval e1) + (eval e2)
  | eval (Negate e1) = ~ (eval e1)
  | eval (Multiply(e1,e2)) = (eval e1) + (eval e2)


 (* In general *)
fun f x =
  case x of
    p1 => e1
    | p2 => e2

(* Can be written as *)

fun f p1 = e1
  | f p2 = e2
  (* ... *)
  | f pn = en
