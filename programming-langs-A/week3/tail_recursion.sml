(* TAIL RECURSION *)

(* the caller, when calls factorial(n-1) in the else branch, has more work to 
do, has to multiply the result by n.
This causes that multiple stack-frames may be calls to the same function
 *)
fun factorial n = if n=0 then 1 else n * factorial(n-1)
val x = factorial 3

(* with tail recursion *)
fun factorial2 n =
  let fun auxiliar(n, accummulator) =
    if n=0
    then accummulator
    else auxiliar(n - 1, accummulator * n)
  in
    auxiliar(n,1)
  end
  
(* this function does the same, calculates de factorial of a number but in a 
more complicated way. It's still recursive. On top of that, the result of 
recursive calls is the result for the caller (no remaining multiplication) *)

(* Tail recursion is more optimal
ML recognizes these tail calls in the compiler and treats them
differently:
– Pop the caller before the call, allowing callee to reuse the
same stack space
– (Along with other optimizations,) as efficient as a loop

There is a methodology that can often guide this transformation:
– Create a helper function that takes an accumulator
– Old base case becomes initial accumulator
– New base case becomes final accumulator
 *)



fun sum xs =
  case xs of
    [] => 0
    | x::xs’ => x + sum xs’

fun sum2 xs =
  let fun aux(xs,acc) =
  case xs of
    [] => acc
    | x::xs’ => aux(xs’,x+acc)
  in
    aux(xs,0)
  end


fun rev xs =
  case xs of
    [] => []
    | x::xs’ => (rev xs’) @ [x]

fun rev2 xs =
  let fun aux(xs,acc) =
  case xs of
    [] => acc
    | x::xs’ => aux(xs’,x::acc)
  in
    aux(xs,[])
  end
