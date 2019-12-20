(* OPTIONS *)

(* this function was created in "let efficiency" part. *)
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

(* we want to remove "if null xs then 0" because we shouldn't accept
emtpy lists when computing "better_max()"" *)

(* this will return an int option *)
fun max1(xs: int list) =
  if null xs
  then NONE
  else 
    let val tl_answer = max1(tl xs)
    in if isSome tl_answer andalso valof tl_answer > hd xs
      then tl_answer
      else SOME (hd xs)
    end 

(*  *)
fun max2(xs: int list) = 
  if null xs
  then NONE
  else let (* fine to assume argument nonempty because it is local *)
           (* int list -> int  *)
          fun max_nonempty(xs: int list) =
            if null (tl xs) (* xs better not be [] *)
            then hd xs 
            else let val tl_answer = max_nonempty(tl xs)
                 in 
                   if hd xs > tl_answer
                   then hd xs
                   else tl_answer
                 end 
        in 
          SOME (max_nonempty xs)
        end 
     