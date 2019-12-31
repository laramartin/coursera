datatype my_type = TwoInts of int * int
                 | Str of string
                 | Pizza;

fun match_pattern(x) =
  case x of 
      Pizza => "This is a Pizza"
    | TwoInts(a, b) => (a + b)
    | Str s => String.size s


match_pattern(Pizza);
match_pattern(TwoInts(3,5));
match_pattern(Str "hello");


datatype student = StudentNumber of int
                | Name of string ;


fun eval e =
  case e of
     Constant i => i
   | Negate e2 => ~ (eval e2)
   | Add(e1,e2) => (eval e1) + (eval e2)
   | Multiply(e1,e2) => (eval e1) * (eval e2)

fun number_of_adds e =
  case e of 
      Constant i => i
    | Negate e2 => number_of_adds e2
    | Add(e1, e2) => 1 + number_of_adds e1 + number_of_adds e2
    | Multiply(e1, e2) => number_of_adds e1 * e2


fun max_constant e =
  case e of 
      Constant i => i
    | Negate e2 => max_constant e2
    | Add(e1, e2) => if max_constant e1 > max_constant e2 
                     then max_constant e1
                     else max_constant e2
    | Multiply(e1, e2) => if max_constant e1 > max_constant e2 
                     then max_constant e1
                     else max_constant e2

                

fun max_constant e =
  case e of 
      Constant i => i
    | Negate e2 => max_constant e2
    | Add(e1, e2) =>
        let val m1 = max_constant e1 
          val m2 = max_constant e2 
        in if m1 > m2 then m1 else m2 end 
    | Multiply(e1, e2) => 
        let val m1 = max_constant e1 
          val m2 = max_constant e2 
        in if m1 > m2 then m1 else m2 end 


fun max_constant e =
  let fun max_of_two(e1, e2) =
     let val m1 = max_constant e1 
          val m2 = max_constant e2 
        in if m1 > m2 then m1 else m2 end 
  in 
    case e of 
        Constant i => i
        | Negate e2 => max_constant e2
        | Add(e1, e2) => max_of_two(e1, e2)
        | Multiply(e1, e2) => max_of_two(e1, e2) 
  end 



fun max_constant e =
  let fun max_of_two(e1, e2) =
     let val m1 = max_constant e1 
          val m2 = max_constant e2 
        in Int.max(m1, m2) end  (*using ML standard library*)
  in 
    case e of 
        Constant i => i
        | Negate e2 => max_constant e2
        | Add(e1, e2) => max_of_two(e1, e2)
        | Multiply(e1, e2) => max_of_two(e1, e2) 
  end 

fun max_constant e =
  let fun max_of_two(e1, e2) =
        (* it will evaluate the values in the function (e.g.max_constant e1) 
        before it calls the function Int.max() *)
        Int.max(max_constant e1, max_constant e2)
  in 
    case e of 
        Constant i => i
        | Negate e2 => max_constant e2
        | Add(e1, e2) => max_of_two(e1, e2)
        | Multiply(e1, e2) => max_of_two(e1, e2) 
  end 