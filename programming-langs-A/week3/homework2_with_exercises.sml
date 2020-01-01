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

(* 
Write a function get_substitutions1, which takes a string list list (a list of 
list of strings, the substitutions) and a string s and returns a string list. 
The result has all the strings that are in some list in substitutions that also 
has s, but s itself should not be in the result. Example:
get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
     (* answer: ["Fredrick","Freddie","F"] *)

Assume each list in substitutions has no repeats. The result will have 
repeats if s and another string are both in more than one list in substitutions.

Example:
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")
(* answer: ["Jeffrey","Geoff","Jeffrey"] *)
Use part (a) and ML’s list-append (@) but no other helper functions. 
Sample solution is around 6 lines.

 *)

(* string list list * string -> string list *)
fun get_substitutions1(x, y) = 
  case x of 
    [] => []
    | x'::x'' => 
                    case all_except_option(y, x') of 
                      NONE => get_substitutions1(x'', y) 
                      | SOME z => z @ get_substitutions1(x'', y)

  
get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
  = ["Fredrick","Freddie","F"];
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")
  = ["Jeffrey","Geoff","Jeffrey"];
get_substitutions1 ([["foo"],["there"]], "foo") = [];

get_substitutions1([["a", "b"], ["a", "c", "d"], ["e"]], "a") = ["b", "c", "d"];
get_substitutions1([["a", "b"], ["a", "c", "d"], ["e"]], "a") = ["b", "c", "d"];
get_substitutions1([["a", "b"], ["a", "b", "c"], ["b"]], "a") = ["b", "b", "c"];
get_substitutions1([["a", "b"], ["a", "b", "c"], ["b"]], "z") = [];

(* (c) Write a function get_substitutions2, which is like get_substitutions1 
except it uses a tail-recursive local helper function. *)
fun get_substitutions2(x, y) = 
  let fun aux(x, acc) =
    case x of 
      [] => acc
      | x'::x'' => 
                    case all_except_option(y, x') of 
                      NONE => aux(x'', acc) 
                      | SOME z => aux(x'', acc @ z)
  in 
    aux(x, [])
  end

get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
  = ["Fredrick","Freddie","F"];
get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")
  = ["Jeffrey","Geoff","Jeffrey"];
get_substitutions2 ([["foo"],["there"]], "foo") = [];

get_substitutions2([["a", "b"], ["a", "c", "d"], ["e"]], "a") = ["b", "c", "d"];
get_substitutions2([["a", "b"], ["a", "c", "d"], ["e"]], "a") = ["b", "c", "d"];
get_substitutions2([["a", "b"], ["a", "b", "c"], ["b"]], "a") = ["b", "b", "c"];
get_substitutions2([["a", "b"], ["a", "b", "c"], ["b"]], "z") = [];


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
