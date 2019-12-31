(* compound type *)
(* with type inference *)
(* order doesn't matter *)
val example = { 
          a = (1 + 2, true andalso true),
          b = 3 + 4,
          c = (false, 9)
        };
    
(* To get a field: 
  #a example;
*)