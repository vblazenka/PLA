(* Programming Languages, Dan Grossman *)
(* Section 2: Datatype Bindings *)

(* one of type *)

datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

val name = Str "John Doe"
val capricosa = Pizza
val nums = TwoInts (4, 3) 