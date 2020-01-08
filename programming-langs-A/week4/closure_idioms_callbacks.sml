val callbacks : (int -> unit) list ref = ref []

fun onKeyEvent f = callbacks := functions :: (!callbacks)

fun onEvent i =
  let fun loop functions =
  case functions of
    [] => ()
    | function::functions’ => (function i; loop functions)
  in loop (!callbacks) end


val timesPressed = ref 0
val _ = onKeyEvent (fn _ => timesPressed := (!timesPressed) + 1)

fun printIfPressed i =
 onKeyEvent (fn j =>
   if i=j
   then print ("pressed " ^ Int.toString i)
   else ())
