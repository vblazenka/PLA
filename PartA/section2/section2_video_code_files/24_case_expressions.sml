(* Programming Languages, Dan Grossman *)
(* Section 2: Case Expressions *)

datatype mytype = TwoInts of int * int 
                | Str of string 
                | Pizza

fun f x =
  case x of
      Pizza => 3
    | Str s => 8
    | TwoInts(i1, i2) => i1 + i2


(* f Pizza
f (Str "Vedran")
f (TwoInts(5, 4)) *)