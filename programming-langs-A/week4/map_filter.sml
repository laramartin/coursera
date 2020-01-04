(* ('a -> 'b) * 'a list -> 'b list *)
fun map (f,xs) =
  case xs of
    [] => []
    | x::xs’ => (f x)::(map(f,xs’));

map((fn x=> x+1), [4,7,12,16]) = [5,9,13,17];
map(hd, [[1,2], [3,4], [5,6,7]]) = [1,3,5];

(* ('a -> bool) * 'a list -> 'a list *)
fun filter (f,xs) =
  case xs of
    [] => []
    | x::xs’ => if f x
                then x::(filter(f,xs’))
                else filter(f,xs’)

(* Returning a function *)
fun double_or_triple f =
  if f 7
  then fn x => 2 * x
  else fn x => 3 * x
(* 
Has type (int -> bool) -> (int -> int)
 But the REPL prints (int -> bool) -> int -> int
because it never prints unnecessary parentheses and
 t1 -> t2 -> t3 -> t4 means t1->(t2->(t3->t4))
 *)
