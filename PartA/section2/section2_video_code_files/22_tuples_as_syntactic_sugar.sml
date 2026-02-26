(* Programming Languages, Dan Grossman *)
(* Section 2: Tuples as Syntactic Sugar *)

(* records are like tuples with user-defined field names
   conversely, tuples are just records with names 1, 2, ..., n
   the only real difference is "by position" vs. "by name"
*)

val a_pair = (3+1, 4+2);
val a_record = {first=3+1, second=4+2};

(* syntax sugar -> repl output is: (6,7) : int * int *)
val another_pair = {2=5, 1=6}; 

#1 a_pair + #2 another_pair;