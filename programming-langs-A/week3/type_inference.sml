(* TYPE INFERENCE 
When not using # character to get fields from a type, 
we don't need to explicitly add the type. 
 *)

fun sum_triple(x, y, z) =
    x + y + z
(* val sum_triple = fn : int * int * int -> int 
compiler knows we expect int because we are adding *)


fun full_name { first = x, middle = y, last = z } =
      x ^ " " ^ y ^ " " ^ z

(* val full_name = fn : {first:string, last:string, middle:string} -> string
compiler knows we expect string because we are concatenating
 *)

 fun sum_triple2(x, y, z) =
    x + z

(* val sum_triple2 = fn : int * 'a * int -> int
compiler gives type 'a to y because we aren't using it
so it could be different types. 
 *)

