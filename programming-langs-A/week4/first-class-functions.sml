(* FUNCTIONAL PROGRAMMING
- avoiding mutation
- using functions as values 
 *)

(* FUNCTIONS AS ARGUMENTS *)

fun increment_n_times1(n,x) =
   if n=0
   then x
   else 1 + increment_n_times(n-1,x)

fun double_n_times1(n,x) = 
   if n=0
   then x
   else 2 * double_n_times1(n-1,x)

fun nth_tail1(n, xs) = 
  if x=0
  then xs
  else tl(nth_tail1(n-1,xs))

(* the 3 previous functions have the same structure, which could be 
extracted as a high-orger function like:  *)

(* n_times = fn : ('a -> 'a) * int * 'a -> 'a *)
fun n_times (f,n,x) =
  if n=0
  then x
  else f (n_times(f,n-1,x))

(* where f is the different computation on each function *)

fun double x = x + x
fun increment x = x + 1

val x1 = n_times(double,4,7)        (*'a is int *)
val x2 = n_times(increment,4,7)     (*'a is int *)
val x3 = n_times(tl,2,[4,8,12,16])  (*'a is int list *)
(* this last one gives an int list and returns an int list *)

fun double_n_times (n,x) = n_times(double,n,x)
fun nth_tail (n,x) = n_times(tl,n,x)


(* ANONYMOUS FUNCTIONS 
- they can't be recursive
- only use it when a function is only used in one place
*)

fun triple_n_times(n,x) =
  n_times(fun triple x = 3*x, n, x)
(* we want to pass a function as an argument. This won't compile, because
the function is a binding, not an expression. But we can use anonymous 
functions: 
*)

fun triple_n_times(n,x) = 
  n_times((fn x => 3*x), n, x) 

(* where "fn x => 3*x) is an anonymous function *)

