(* Programming Languages, Dan Grossman *)
(* Section 2: Records *)

(* is each-of type *)
val x = { bar = (1+2, true andalso true), foo = 3+4, baz = (false, 9) };

val my_niece = { nam = "Amelia", id = 41123 - 12 };

#id my_niece;