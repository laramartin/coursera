(* CLOSURE IDIOM: COMBINING FUNCTIONS *)

fun compose(f, g) = fn x => f(g(x))

(* ('b -> 'c) * ('a -> 'b) -> ('a -> 'c) *)

(* it's the same as writing: "f o g" *)

fun sqrt_of_abs i = Math.sqrt(Real.fromInt(abs i))
fun sqrt_of_abs i = (Math.sqrt o Real.fromInt o abs) i
val sqrt_of_abs = Math.sqrt o Real.fromInt o abs
 
 (* 
 “Pipelines” of functions are common in functional programming and
many programmers prefer left-to-right
– Can define our own infix operator
– This one is very popular (and predefined) in F#
  *)

infix |>
fun x |> f = f x
fun sqrt_of_abs i =
 i |> abs |> Real.fromInt |> Math.sqrt
