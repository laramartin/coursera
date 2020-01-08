(* CLOSURE IDIOM: CURRYING *)

(* way to deal with "multi argument" functions in ML *)

val sorted3 = fun x => fn y => fn z => z >= y andalso y >= x;

((sorted3 7) 9) 11

(* 
Calling (sorted3 7) returns a closure with:
– Code fn y => fn z => z >= y andalso y >= x
– Environment maps x to 7
• Calling that closure with 9 returns a closure with:
– Code fn z => z >= y andalso y >= x
– Environment maps x to 7, y to 9
• Calling that closure with 11 returns true
 *)

(* passing multiple patterns like x y z separated by a space, 
before the equals, means currying function. 
sorted3_nicer() is syntactic sugar of sorted3()
 *)
fun sorted3_nicer x y z = z >= y andalso y >= x;

(* we call it like this *)
sorted3_nicer 7 9 11 

(* also like this *)
((sorted3_nicer(7) 9) 11)


(* CURRIED FOLD *)
fun fold f acc xs =
  case xs of
    [] => acc
    | x::xs’ => fold f (f(acc,x)) xs’;

fun sum xs = fold (fn (x,y) => x+y) 0 xs;

(* PARTIAL APPLICATION
 if caller provides “too few” arguments, we get back a closure
“waiting for the remaining arguments”
 *)

fun sum_inferior xs = fold (fn (x,y) => x+y) 0 xs
val sum = fold (fn (x,y) => x+y) 0

(* 
As we already know, fold (fn (x,y) => x+y) 0
evaluates to a closure that given xs, evaluates the case-expression
with f bound to fold (fn (x,y) => x+y) and acc bound to 0
 
Unnecessary function wrapping

- Previously learned not to write fun f x = g x
when we can write val f = g
- This is the same thing, with fold (fn (x,y) => x+y) 0 in
place of g
*)

fun exists predicate xs =
  case xs of
    [] => false
    | x::xs’ => predicate x orelse exists predicate xs’

val no = exists (fn x => x=7) [4,11,23]
val hasZero = exists (fn x => x=0)
