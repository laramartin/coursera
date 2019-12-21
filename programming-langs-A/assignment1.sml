(* date -> int * int * int *)
(* date -> year, month, day *)
(* day of year -> 1 to 365. E.g. 33 = Feb 2 *)
(* 
val firstDate = (2019, 2, 5);
val secondDate = (2019, 5, 10); *)

(* exercise 1 *)
(* true if first comes before than second *)
fun is_older(firstDate: (int*int*int), secondDate: (int*int*int)) =
  if (#1 firstDate <> #1 secondDate) 
  then (#1 firstDate < #1 secondDate) 
  else if (#2 firstDate <> #2 secondDate)
  then (#2 firstDate < #2 secondDate)
  else (#3 firstDate < #3 secondDate);

(* is_older((1,1,1), (2,2,2)); (* true *)
is_older((1,1,1), (1,1,1)); (* false *)
is_older((1,1,1), (1,1,2)); (* true *)
is_older((1,1,1), (1,2,1)); (* true *)  
is_older((2,2,2), (1,1,1)); (* false *)
is_older((2,2,2), (2,1,3)); (* false *) 
is_older((2,2,2), (2,2,1)); false *)

(* exercise 2 *)
(* returns how many dates in the list are in the given month *)
fun number_in_month(dates: (int * int * int) list, month: int) =
  if null dates
  then 0
  else if ((#2 (hd dates)) = month)
    then 1 + number_in_month(tl dates, month)
    else 0 + number_in_month(tl dates, month)

(* val dates = [(1,2,3), (4,5,6)];
number_in_month(dates, 2); (* 1 *)
number_in_month(dates, 1); (* 0 *)
number_in_month(dates, 5); 1 *)

(* exercise 3 *)
fun number_in_months(dates: (int * int * int) list, months: int list) =
  if (null dates orelse null months)
  then 0
  else (number_in_month(dates, hd months) + number_in_months(( dates, tl months)))

(* 
number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) (* 3 *) *)

(* val dates = [(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)];  *)
(* val months =[2,3,4]; *)

(* exercise 4 *)
(* returns a list holding the dates from the argument list of dates that are in the month *)
fun dates_in_month(dates: (int * int * int) list, month: int) =
  if null dates
  then [] 
  else if (((#2 (hd dates)) = month)) 
        then (hd dates) :: dates_in_month(tl dates, month) 
        else dates_in_month(tl dates, month)

(* dates_in_month([(2012,2,28),(2013,12,1)],2); [(2012,2,28)]  *)

(* exercise 5*)
(* returns a list holding the dates from the argument list of dates that are in any of the months in the list of months *)
fun dates_in_months(dates: (int * int * int) list, months: int list) =
  if (null dates orelse null months)
  then []
  else (dates_in_month(dates, hd months) @ dates_in_months(( dates, tl months)))

(* exercise 6 *)
(* returns the n^th element of the list where the head of the list is 1st *)
fun get_nth(elements: string list, number: int) = 
  if (number = 1)
  then (hd elements)
  else get_nth(tl elements, number -1)

(* exercise 7 *)
(* returns string from of January 20, 2013 *)
val monthNames = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];

fun date_to_string(date: (int * int * int)) = 
  get_nth(monthNames, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

(* exercise 8 *)
(* returns an int such that the first n elements of the list add to less than sum, but the first n + 1 elements of the list add to sum or more.  *)
fun number_before_reaching_sum(sum: int, numbers: int list) = 
  if (hd numbers + hd(tl numbers)) >= sum 
  then 1
  else 1 + number_before_reaching_sum(sum - (hd numbers), (tl numbers)) 

(* exercise 9 *)
(* Write a function what_month that takes a day of year (i.e., an int between 1 and 365) 
and returns what month that day is in (1 for January, 2 for February, etc.). Use a list holding 
12 integers and your answer to the previous problem. *)

(* val days_in_months = [31, 30, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]; *)

fun what_month(dayOfYear: int) =
  let val days_in_months = [31, 30, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in 
    if (dayOfYear > 31) 
    then 1 + number_before_reaching_sum(dayOfYear, days_in_months)
    else number_before_reaching_sum(dayOfYear, days_in_months)
  end

(* I need exercise 9 for this one *)
(* exercise 10 *)
(* returns an int list [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2. *)
fun month_range(day1: int, day2: int) = 
  let val result = ([what_month(day1)])
  in 
    if (day1 + 1) > day2
    then result
    else if day1 = day2
    then result @ [what_month(day2)]  (*here I should return result :\ *)
    else result @ month_range( day1 + 1, day2)
  end

(* exercise 11 *)
(* takes a list of dates and evaluates to an (int*int*int) option. It evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)
fun oldest(dates: (int * int * int) list) = 
  if null dates
  then NONE
  else  let fun oldest_nonempty(dates: (int * int * int) list) = 
              if null (tl dates) (* if list length is 1 *)
              then hd dates 
              else let val tl_answer = oldest_nonempty(tl dates)
                  in 
                    if is_older(hd dates, tl_answer)
                    then hd dates
                    else tl_answer
                  end  
        in 
          SOME (oldest_nonempty(dates))
        end

(* oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) SOME (2011,3,31) *)