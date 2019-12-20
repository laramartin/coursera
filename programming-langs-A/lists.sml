fun sum_list(numbers : int list) =
  if null numbers
  then 0
  else hd numbers + sum_list(tl numbers)

(* list_product *)
fun list_product(numbers : int list) = 
  if null numbers
  then 1
  else hd numbers * list_product(tl numbers)


val x = list_product[]; (* 1 *)
val y = list_product[5]; (* 5 *)
val z = list_product[2, 4, 2]; (* 16 *)


(* [7, 6, 5, 4, 3, 2, 1] *)
fun countdown (number : int) = 
  if x = 0
  then []
  else x :: (countdown(x - 1))

  fun append (firstList : int list, secondList : int list) =
    if null firstList
    then secondList
    else (hd firstList) :: append((tl firstList), secondList)

(* (int list) * (int list) -> int list *)

(* functions over pairs of lists *)

fun sum_pair_list (xs : (int * int) list) =
  if null xs 
  then 0
  else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs)

fun firsts (xs : (int*int) list) =
 if null xs
 then []
 else #1(hd xs) :: firsts(tl xs)

fun seconds (xs : (int*int) list) =
 if null xs
 then []
 else #2(hd xs) :: seconds(tl xs)

fun sum_pair_list2 (xs : (int*int) list) =
  (sum_list (firsts xs)) + (sum_list (seconds xs))

val x = [(3,4), (5,6)]
(* sum_pair_list [(3,4), (5,6)]*)
(* first(x) -> [3, 5] *)
(* second(x) -> [4, 6] *)
