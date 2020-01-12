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
        (fn(a,b) => if ((String.size a) = (String.size b)) then a 
                    else if ((String.size a) > (String.size b)) then a 
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

(* (int * int -> bool) -> string list -> string *)
(* fun longest_string_helper =; *)

(*)
longest_string3 ["A","bc","C"] = "bc";
longest_string3 [] = "";
longest_string3 ["a"] = "a";
longest_string3 ["a", "b", "c"] = "a";;
*)
(* val test4b = longest_string4 ["A","B","C"] = "C"; *)


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
use "homework3_with_exercises.sml";
 *)
