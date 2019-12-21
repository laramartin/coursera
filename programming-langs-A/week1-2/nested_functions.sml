fun count (from: int, to: int) = 
  if from = to
  then to :: []
  else from :: count (from +1, to)

fun count_up_from_one(x: int) =
  count(1, x) 

(* create count() in scope of count_up_from_one *)
fun count_up_from_one(x: int) =
  let
    fun count (from: int, to: int) = 
      if from = to
      then to :: []
      else from :: count (from +1, to)
  in
    count(1, x)
  end

(* improve count_up_from_one. It's parameter "x" is what we're passing to the count() as "to"
so we can replace "to" by "x" *)
fun count_up_from_one(x: int) =
  let
    fun count (from: int, x: int) = 
      if from = x
      then x :: []
      else from :: count (from +1, x)
  in
    count(1, x)
  end

(* we can improve count_up_from_one even more. 
It's parameter "x" is a binding in the scope, so we don't really
need to pass it to count(), we can use it directly. Hence we can remove
"x" from the count(): *)
fun count_up_from_one(x: int) =
  let
    fun count (from: int) = 
      if from = x
      then x :: []
      else from :: count (from +1)
  in
    count(1)
  end