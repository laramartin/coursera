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

(* 
(d) Write a function similar_names, which takes a string list list 
of substitutions (as in parts (b) and (c)) and a full name of 
type {first:string,middle:string,last:string} and returns a list of full 
names (type {first:string,middle:string,last:string} list). The result is all 
the full names you can produce by substituting for the first name (and only 
the first name) using substitutions and parts (b) or (c). The answer should 
begin with the original name (then have 0 or more other names). Example:
similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})
     (* answer: [{first="Fred", last="Smith", middle="W"},
                 {first="Fredrick", last="Smith", middle="W"},
                 {first="Freddie", last="Smith", middle="W"},
                 {first="F", last="Smith", middle="W"}] *)
Do not eliminate duplicates from the answer. Hint: Use a local helper function. 
Sample solution is around 10 lines.
*)

type Name = { first: string, middle: string, last: string };

fun process_names(n, y) =
  case y of
  {first:string, middle:string, last:string} => 
    case n of
      [] => []
      | n'::n'' => {first=n', middle=middle, last=last} :: process_names(n'', y)
 ;

(* string list list * name list -> *)
fun similar_names(x, y) = 
  case y of 
    {first, ...} => 
      y :: process_names(get_substitutions2(x, first), y);

(* get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") 
= ["Fredrick","Freddie","F"]   

result of similar_names would be: 
- name 
- for each result of get_substitutions2
    first = head of result, middle, and last 

*)

similar_names([], {first="Fred", last="Smith", middle="W"}) 
  = [{first="Fred",last="Smith",middle="W"}];
similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [
            {first="Fred", last="Smith", middle="W"}, 
            {first="Fredrick", last="Smith", middle="W"},
	        {first="Freddie", last="Smith", middle="W"}, 
            {first="F", last="Smith", middle="W"}
        ];

similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="", last=""}) =
	    [
            {first="Fred", last="", middle=""}, 
            {first="Fredrick", last="", middle=""},
	        {first="Freddie", last="", middle=""}, 
            {first="F", last="", middle=""}
        ];

similar_names ([["Fred","Fredrick"],["a","b"],["c","d","ef"]], {first="Fred", middle="W", last="Smith"}) =
	    [
            {first="Fred", last="Smith", middle="W"}, 
             {first="Fredrick", last="Smith", middle="W"}
         ];

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* 
- game played with a s card list and a goal. 
- held-cards is initially empty
- user moves: 
    1) drawing: removing the first card in the card-list and adding it to the 
    held-cards
    2) discarding: choosing one of the held-cards to remove
- game ends:
    1) player chooses to make no more moves
    2) when the sum of the values of the held-cards is greater than the goal
- objective: End the game with a low score. 0 is best .
- scoring: 
    - sum: sum of the values of the held-cards
    - sum > goal: preliminary score is three times (sum - goal)
      else preliminary score is (goal - sum )
    - score is preliminary score unless all the held-cards are the same color, 
      in which case the score is the preliminary score divided by 2 
      (rounded down as usual with integer division, using ML's "div" operator)
*)


(* (a) Write a function card_color, which takes a card and returns its color (spades and clubs are black, diamonds 
and hearts are red). Note: One case-expression is enough. *)

fun card_color(c) = 
   case c of 
     (Spades, _) => Black
     | (Clubs, _) => Black 
     | (Diamonds, _) => Red
     | (Hearts, _) => Red


(* (b) Write a function card_value, which takes a card and returns its value 
(numbered cards have their number as the value, aces are 11, everything else 
is 10). Note: One case-expression is enough. *)

(* datatype rank = Jack | Queen | King | Ace | Num of int  *)
fun card_value(c) =
  case c of 
    (_, Num n) => n
    | (_, Ace) => 11
    | (_, _) => 10

card_value(Clubs, Num 2) = 2;
card_value(Spades, Ace) = 11;
card_value(Hearts, Queen) = 10;
card_value(Diamonds, King) = 10;
card_value(Diamonds, Num 1) = 1;

(* (c) Write a function remove_card, which takes a list of cards cs, a card c, 
and an exception e. It returns a list that has all the elements of cs except c. 
If c is in the list more than once, remove only the first one. If c is not in 
the list, raise the exception e. You can compare cards with =.  *)

fun is_same_card(c1, c2) = 
  case c1 of 
    (x, x') => case c2 of 
                (y, y') => if (x, x') = (y, y') then true else false

is_same_card((Hearts, Ace), (Hearts, Ace)) = true;
is_same_card((Hearts, Ace), (Hearts, Num 1)) = false;

fun is_card_in_list(cs, c) = 
  case cs of 
    [] => false
    | x::[] => if is_same_card(x, c) then true else false
    | x::y => if is_same_card(x, c) 
                    then true
                    else is_card_in_list(y,c)

is_card_in_list([], (Hearts, Ace)) = false;
is_card_in_list([(Hearts, Ace)], (Hearts, Ace)) = true;
is_card_in_list([(Diamonds, Num 1), (Hearts, Ace)], (Hearts, Ace)) = true;
is_card_in_list([(Diamonds, Num 1), (Diamonds, Num 1)], (Hearts, Ace)) = false; 

(* (c) Write a function remove_card, which takes a list of cards cs, a card c, 
and an exception e. It returns a list that has all the elements of cs except c. 
If c is in the list more than once, remove only the first one. If c is not in 
the list, raise the exception e. You can compare cards with =.  *)
fun remove_card(cs, c, e) =
    case cs of
    [] => raise e
    | x::y => if x = c then y else x :: remove_card(y, c, e);
   
  

(* (* errors: 
- removes all instances of c in cs
- when reaching last item, raises e 
 *)
fun remove_card2(cs, c, e) =
  case cs of 
    [] => raise e 
    | x::[] => if is_same_card(x, c) then [] else raise e 
    | x::y =>  if is_same_card(x, c) 
                    then remove_card(y, c, e)
                    else (x :: remove_card(y, c, e)) *)

remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [];

remove_card([], (Hearts, Ace), IllegalMove); (* = IllegalMove; *)
remove_card([(Hearts, Ace)], (Clubs, Num 1), IllegalMove); (* = IllegalMove; *)

remove_card([(Hearts, Ace), (Clubs, Num 2), (Clubs, Num 2), (Clubs, Num 2)], (Hearts, Ace), IllegalMove) 
  = [(Clubs, Num 2), (Clubs, Num 2), (Clubs, Num 2)];

remove_card([(Hearts, Ace), (Clubs, Num 2), (Clubs, Num 2), (Clubs, Num 2)], (Clubs, Num 2), IllegalMove) 
  = [(Hearts, Ace), (Clubs, Num 2), (Clubs, Num 2)];

remove_card([(Hearts, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove) 
  = [(Hearts, Ace)];

(* 

(d) Write a function all_same_color, which takes a list of cards and returns 
true if all the cards in the list are the same color. Hint: An elegant solution 
is very similar to one of the functions using nested pattern-matching in the 
lectures.
 *)

fun all_same_color(cs) =
  case cs of 
    [] => true
    | x::y => case y of 
                [] => true  
                | y'::y'' => if (card_color(x) = card_color(y'))
                            then all_same_color(y) 
                            else false
                
all_same_color [(Hearts, Ace), (Hearts, Ace)] = true;
all_same_color [(Clubs, Ace), (Hearts, Ace)] = false;
all_same_color [] = true;
all_same_color [(Clubs, Ace)] = true;
all_same_color [(Clubs, Ace), (Clubs, Ace), (Clubs, Ace), (Clubs, Ace)] = true;

(* (e) Write a function sum_cards, which takes a list of cards and returns 
the sum of their values. Use a locally defined helper function that is tail 
recursive. (Take “calls use a constant amount of stack space” as a requirement 
for this problem.) *)

fun sum_cards(cs) =
  let fun aux(cs, acc) =
    case cs of 
     [] => acc
     | cs'::[] => acc + card_value(cs') 
     | cs'::cs'' => aux(cs'', (acc + card_value(cs')))
  in
    aux(cs, 0)
  end

sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
sum_cards [(Clubs, Num 10),(Clubs, Num 2)] = 12;
