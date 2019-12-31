(* 
''a list * ''a -> bool

''a means that we can only replace a with "equality types", that
is types we get using the = operator
 *)

fun same_thing(x, y) = 
  if x=y then "yes" else "no"


(*
we can ignore the warning 
stdIn:19.7 Warning: calling polyEqual
val same_thing = fn : ''a * ''a -> string
 *)