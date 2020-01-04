(* LEXICAL SCOPE
function bodies can use any bindings in scope, the 
scope where the function was defined. 

1) Function meaning does not depend on variable names used.
2) Functions can be type-checked and reasoned about where defined.
3) Closures can easily store the data they need.
 *)

(* 1 *) val x = 1
(* 2 *) fun f y = x + y
(* 3 *) val x = 2
(* 4 *) val y = 3
(* 5 *) val z = f (x + y)

(* 
Call on line 5:
– Looks up f to get the function defined on line 2
– Evaluates x+y in current environment, producing 5
– Calls the function with 5, which evaluates the body in the old
environment, producing 6
 *)

(* CLOSURES
functions have 2 parts: 
- code
- environment -> was current when the function was defined
 *)



fun fold (f,acc,xs) =
  case xs of
    [] => acc
    | x::xs => fold(f, f(acc,x), xs)


(* These are useful and do not use “private data” *)
fun f1 xs = fold((fn (x,y) => x+y), 0, xs)

fun f2 xs = fold((fn (x,y) => x andalso y>=0),
 true, xs)

 (* These are useful and do use “private data” *)
 fun f3 (xs,hi,lo) =
    fold(fn (x,y) =>
        x + (if y >= lo andalso y <= hi
            then 1
            else 0)),
        0, xs)

fun f4 (g,xs) = fold(fn (x,y) => x andalso g y), true, xs)
