(* OPTIONS ARE DATATYPES 
NONE and SOME are type constructors! They take
types
Better use pattern-matching than isSome and valOf
*)

fun inc_or_zero(intoption) =
  case intoption of
          NONE => 0
        | SOME i => i + 1


(* LISTS ARE DATATYPES,
do not use hd, tl or null either:

[] and :: are constructors too
 *)

 fun sum_list(xs) =
    case xs of 
         [] => 0
        | x::xs' => x + sum_list xs'

fun append(xs, ys) =
    case xs of 
         [] => ys
        | x::xs' => x :: append(xs', ys)


(* Better use pattern-matching because because we'll check all datatypes and won't forget cases  *)