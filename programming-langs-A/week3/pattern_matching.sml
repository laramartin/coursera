fun sum_triple(triple) =
  case triple of
    (x, y, z) => x + y + z

(* since the operation we do is addition, the parameters
will have to be ints, therefore the type expected by this 
function is (int * int * int) *)

fun full_name(record) =
  case record of 
    { first = x, middle = y, last = z } => 
      x ^ " " ^ y ^ " " ^ z

(* the operation we are doing is ˆ, which concatenates
strings. Therefore, the type of x, y and z is string *)

(* FUNCTION-ARGUMENT PATTERNS *)
(* we can pass the pattern as the argument *)

(* sum_triple2 looks exactly the same as sum_triple. How do we know
that the function is taking a tuple of 3 types and not 3 different arguments??
well... functions in ML only take 1 argument!!! 
We've been passing patterns until know without knowing...🤯 *)
fun sum_triple2(x, y, z) =
    x + y + z

fun full_name2 { first = x, middle = y, last = z } =
      x ^ " " ^ y ^ " " ^ z

(* How many arguments does the following function take? *)
fun hello() = print "Hello World!\n";

(* Every function takes exactly one argument.
In this case, the argument is of type 𝚞𝚗𝚒𝚝 and () is a pattern to 
match the only value of type 𝚞𝚗𝚒𝚝. *)
