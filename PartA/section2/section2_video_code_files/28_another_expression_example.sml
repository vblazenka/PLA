(* Programming Languages, Dan Grossman *)
(* Section 2: Another Expression Example *)

datatype exp = Constant of int 
             | Negate of exp 
             | Add of exp * exp
             | Multiply of exp * exp

(* Note: example as explained in video assumes there is no library function
   for max of two ints.  There is: Int.max *)

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

val test_exp = Add (Constant 19, Negate (Constant 4))
val nineteen = max_constant test_exp

(* Note: Using Int.max, we don't need a local helper function. 
This second version is fewer lines, but there is a bit more
code copying. *)
