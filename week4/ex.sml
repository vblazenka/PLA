(* Exercises *)

(* Functions as Arguments *)
(* Passing one fun as argument to other fn *)

fun n_times(f, n, x) = 
    if n = 0
    then x
    else f(n_times(f, n-1, x));

fun double x = x + x;
val x1 = n_times(double, 4, 7);

(* Polymorphic Types and Functions as Arguments *)