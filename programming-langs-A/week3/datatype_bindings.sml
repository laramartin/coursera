datatype my_type = TwoInts of int * int
                 | Str of string
                 | Pizza;

val a = Str "hi";
val b = Str;
val c = Pizza;
val d = TwoInts(1+2, 3+4);
val e = a;



(* 
val a = Str "hi" : my_type
val b = fn : string -> my_type
val c = Pizza : my_type
val d = TwoInts (3,7) : my_type
val e = Str "hi" : my_type
val it = () : unit
 *)