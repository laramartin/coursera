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
