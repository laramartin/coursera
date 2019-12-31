exception ListLengthMismatch

(* The idea of the zip():

zip([1,2,3], [4,5,6], [7,8,9]);
val it = [(1,4,7), (2,5,8), (3,6,9)];
 *)

(* this is the way we would do it without pattern matching *)
fun old_zip(l1, l2, l3) =
  if null l1 andalso null l2 andalso null l3
  then []
  else if null l1 orelse null l2 orelse null l3 
  then raise ListLengthMismatch
  else (hd l1, hd l2, hd l3) :: old_zip(tl l1, tl l2, tl l3)

(* better way of doing the same *)
fun zip2(lists) =
  case lists of
    ([],[],[]) => []
    | (hd1::tl1,hd2::tl2,hd3::tl3) => (hd1,hd2,hd3)::zip3(tl1,tl2,tl3)
    | _ => raise ListLengthMismatch

(* notes from the slides:  
Pattern a::b::c::d matches all lists with >= 3 elements
Pattern a::b::c::[] matches all lists with 3 elements
Pattern ((a,b),(c,d))::e matches all non-empty lists of
pairs of pairs
 *)

 (* int list -> bool *)
fun nondecreasing xs = 
  case xs of 
    [] => true
    | x::xs' => case xs' of 
                [] => true (* also non decreasing, case where list length is 1*)
                | y::ys' => x <= y andalso nondecreasing xs'

(* we can write the same with a nested pattern *)
fun nondecreasing2 xs = 
  case xs of 
    [] => true
    (* nested pattern matching that handles 1 element list case directly,
    where head of the list matches the first element, and the rest matches [] *)
    | x::[] => true 
    | y::ys' => x <= y andalso nondecreasing xs'


(* ANOTHER EXAMPLE *)

(* sign can be Positive, Negative or Zero *)
datatype sign = P | N | Z 

(* int * int -> sign *)
fun multisign (x1, x2) =
  let fun sign x = if x=0 then Z, else if x>0 then P else N 
  in 
    case (sign x1, sign x2) of 
    (Z,Z) => (**)
    | (P,Z) => (**)
    | (N,Z) => (**)
  end

(* in the case that x1 is Z (zero), the result will be Z (zero), so that 
resolves 3 of the cases: (Z,Z), (Z,P), (Z, N). 
We can improve the function: *)

fun multisign (x1, x2) =
  let fun sign x = if x=0 then Z, else if x>0 then P else N 
  in 
    case (sign x1, sign x2) of 
    (Z,_) => Z
    | (_,Z) => Z
    | (P,P) => P
    | (N,N) => P 
    | _ => N (* the rest of the cases will result in N *)
  end
