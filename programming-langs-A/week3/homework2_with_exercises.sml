(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)


(* takes a string and a string list
Return NONE if the string is not in the list, else return SOME lst 
where lst is identical to the argument list except the string is not in it. 
You may assume the string is in the list at most once. Use same_string, 
provided to you, to compare strings. Sample solution is around 8 lines.
 *)
  
fun all_except_option (x, ys) =
    case (ys) of 
      [] => NONE 
      | y::[] =>  if same_string(x, y)
                  then SOME []
                  else NONE
      | y::y' =>  if same_string(x, y)
                  then SOME y'
                  (* else (y @ all_except_option(x, y')) *)
                  else 
                    case (all_except_option(x, y')) of 
                      NONE => NONE
                      | SOME z => SOME (y :: z)


all_except_option("a", []) = NONE;
all_except_option("a", ["a"]) = SOME [];
all_except_option("a", ["b"]) = NONE; 
all_except_option("a", ["a", "b"]) = SOME ["b"];
all_except_option("a", ["a", "b", "c"]) = SOME ["b", "c"];
all_except_option("b", ["a", "b", "c"]) = SOME ["a", "c"];
all_except_option("c", ["a", "b", "c"]) = SOME ["a", "b"];




(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
