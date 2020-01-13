(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 
1. Write a function only_capitals that takes a string list and returns a 
string list that has only the strings in the argument that start with an 
uppercase letter. Assume all strings have at least 1 character. Use List.filter,
 Char.isUpper, and String.sub to make a 1-2 line solution.

 - List.filter => filter f l applies f to each element x of l, from left to right, and returns the list of those x for which f x evaluated to true, in the same order as they occurred in the argument list.

 - Char.isUpper => isUpper c returns true if c is an uppercase letter.

 - String.sub => sub (s, i) returns the i(th) character of s, counting from zero. This raises Subscript if i < 0 or |s| <= i.
 *)

fun only_capitals(words) = 
   List.filter (fn x => Char.isUpper (String.sub (x, 0))) words;
  

only_capitals ["A","B","C"] = ["A","B","C"];
only_capitals ["a","b","c"] = [];
only_capitals ["Abc"] = ["Abc"];
only_capitals [] = [];

(* 
2. Write a function longest_string1 that takes a string list and returns the 
longest string in the list. If the list is empty, return "". In the case of a 
tie, return the string closest to the beginning of the list. Use foldl, 
String.size, and no recursion (other than the implementation of foldl is 
recursive).

- foldl => List.foldl (op ^) "x" ["a","b","c"] = "cbax"

- String.size => size s returns |s|, the number of characters in string s.
String.size "Lara";
val it = 4 : int
 *)

 (* fn : string list -> string *)
fun longest_string1 xs = 
  List.foldl    
        (fn(a,b) => if ((String.size a) > (String.size b)) then a else b ) 
        "" (* accumulator *)
        xs; 

longest_string1 ["A","bc","C"] = "bc";
longest_string1 [] = "";
longest_string1 ["a"] = "a";
longest_string1 ["a", "b", "c"] = "a";

(* 
3. Write a function longest_string2 that is exactly like longest_string1 except 
in the case of ties it returns the string closest to the end of the list. 
Your solution should be almost an exact copy of longest_string1. Still 
use foldl and String.size.
 *)

(* fn : string list -> string *)
fun longest_string2 xs = 
  List.foldl    
        (fn(a,b) => if ((String.size a) >= (String.size b)) 
                    then a 
                    else b)
        "" (* accumulator *)
        xs; 

longest_string2 ["A","bc","C"] = "bc";
longest_string2 [] = "";
longest_string2 ["a"] = "a";
longest_string2 ["a", "b", "c"] = "c";

(* 
4. Write functions longest_string_helper, longest_string3, and longest_string4 such that:
• longest_string3 has the same behavior as longest_string1 and longest_string4 has the
same behavior as longest_string2.
• longest_string_helper has type (int * int -> bool) -> string list -> string
(notice the currying). This function will look a lot like longest_string1 and 
longest_string2 but is more general because it takes a function as an argument.
• If longest_string_helper is passed a function that behaves like > 
(so it returns true exactly when its first argument is stricly greater than 
its second), then the function returned has the same behavior as longest_string1.
• longest_string3 and longest_string4 are defined with val-bindings and partial 
applications of longest_string_helper.
*)

(* int > int -> bool *)
(* (int * int -> bool) -> string list -> string *)
fun longest_string_helper function = 
    List.foldl    
        (fn(a,b) => if (function (String.size a) (String.size b)) then a else b)
        "";
  

(* same as longest_string1 *)
fun longest_string3 xs = 
  let fun comparator a b = a > b
  in 
    longest_string_helper(comparator) xs
  end;

longest_string3 ["A","bc","C"] = "bc";
longest_string3 [] = "";
longest_string3 ["a"] = "a";
longest_string3 ["a", "b", "c"] = "a";;

fun longest_string4 xs = 
  let fun comparator a b = a >= b
  in 
    longest_string_helper(comparator) xs
  end;

longest_string4 ["A","B","C"] = "C";
longest_string4 ["A","bc","C"] = "bc";
longest_string4 [] = "";
longest_string4 ["a"] = "a";
longest_string4 ["a", "b", "c"] = "c";

(* 5. Write a function longest_capitalized that takes a string list and returns 
the longest string in the list that begins with an uppercase letter, or "" if 
there are no such strings. Assume all strings have at least 1 character. Use a 
val-binding and the ML library’s o operator for composing functions. 
Resolve ties like in problem 2. *)

fun longest_capitalized xs = 
  let fun isCapizalized x = (Char.isUpper (String.sub (x, 0)))
  in 
    List.foldl    
            (fn(a,b) => if isCapizalized a 
                        then 
                            (if ((String.size a) > (String.size b)) 
                            then a   
                            else if isCapizalized b
                            then b
                            else "")
                        else if isCapizalized b 
                        then b 
                        else "")
            "" (* accumulator *)
            xs
  end;

longest_capitalized ["A","bc","C"] = "A";
longest_capitalized ["A","BB","C"] = "BB";
longest_capitalized [] = "";

(* 
6. Write a function rev_string that takes a string and returns the string that 
is the same characters in reverse order. Use ML’s o operator, the library 
function rev for reversing lists, and two library functions in the String 
module. (Browse the module documentation to find the most useful functions.)
 *)

(* 
- input                                                 "abc"
- split string in char list with String.explode         [#"a", #"b", #"c"]
- List.rev                                              [#"c", #"b", #"a"]
- concat all with String.implode                        "cba"                  
 *)

fun rev_string str = (String.implode o List.rev o String.explode) str;

(* 
String.explode "abc" = [#"a", #"b", #"c"];
List.rev [#"a", #"b", #"c"] = [#"c", #"b", #"a"];
String.implode [#"c", #"b", #"a"] = "cba"; *)


(* 
7. Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b 
(notice the 2 arguments are curried). The first argument should be applied 
to elements of the second argument in order until the first time it returns 
SOME v for some v and then v is the result of the call to first_answer. If the 
first argument returns NONE for all list elements, then first_answer should 
raise the exception NoAnswer. Hints: Sample solution is 5 lines and does 
nothing fancy.
 *)

(* (’a -> ’b option) -> ’a list -> ’b  *)

fun first_answer (function) = 
  fn list => 
  case (List.foldl (fn (a, b) => case b of 
                                    NONE => function a
                                    | SOME v => SOME v
                                    ) 
                    NONE 
                    list) of 
                        NONE => raise NoAnswer  
                        | SOME v => v;
                                    
first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4;

(* 
8. Write a function all_answers of type 
(’a -> ’b list option) -> ’a list -> ’b list option (notice the 2 arguments are 
curried). The first argument should be applied to elements of the second argument. 
If it returns NONE for any element, then the result for all_answers is NONE. 
Else the calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn 
and the result of all_answers is SOME lst where lst is lst1, lst2, ..., lstn 
appended together (order doesn’t matter). Hints: The sample solution is 8 lines. 
It uses a helper function with an accumulator and uses @. 
Note all_answers f [] should evaluate to SOME [].
 *)
(* fun all_answers (function) =
    fn list => 
        let 
            fun aux (list, acc) = 
                case list of 
                    [] => acc
                    | x'::x'' => case (List.foldl (function list) aux(list, acc @ x') list) of 
                                    NONE => NONE
                                    | SOME v => SOME v
        in 
            aux(list, NONE)
        end; *)

fun all_answers (function) =
    fn list => 
        let val result =
            List.foldl 
                (function list) 

                let 
                    fun aux (list, acc) = 
                        case list of 
                            [] => acc
                            | x'::x'' => aux(list, acc @ x')
                in 
                aux(list, [])
                end
                
                list

            in 
                case result of 
                    NONE => NONE
                    | SOME v => SOME v
            end
            
            (* case (List.foldl (function list) aux(list, acc @ x') list) of 
                                    NONE => NONE
                                    | SOME v => SOME v *)
        


(* all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE;
all_answers f [] = SOME []; *)

(* 
fun bla f = fn x => f x;
val bla = fn : ('a -> 'b) -> 'a -> 'b 
*)


(* 
use "homework3_with_exercises.sml";
 *)
 
