fun bad_max(xs: int list) =
  if null xs
  then 0
  else if null (tl xs) 
  then hd xs
  else if hd xs > bad_max(tl xs)
  then hd xs
  else bad_max(tl xs)

(* bad_max(tl xs) is being called twice, making this recursion exponential.
To not do repeated work, we can save the recursive result in a 
local variable *)

fun better_max(xs: int list) = 
  if null xs
  then 0 
  else if null (tl xs)
  then hd xs
  else 
    let val result = good_max(tl xs)
    in 
      if hd xs > result 
      then hd xs
      else result 
    end 

