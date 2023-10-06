(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* 1. This problem involves using first-name substitutions to come up with alternate names. For example,
Fredrick William Smith could also be Fred William Smith or Freddie William Smith. Only part (d) is
specifically about this, but the other problems are helpful. *)

(* 1a. Write a function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
to compare strings. Sample solution is around 8 lines.*)
fun all_except_option (str, lst) =
    case lst of
        [] => NONE
        | x::xs' =>
            if same_string(str, x) then SOME xs'
            else
                case all_except_option(str, xs') of
                    NONE => NONE
                  | SOME ys' => SOME (x::ys')

val all_except_option_test1 = all_except_option("a", []) = NONE
val all_except_option_test2 = all_except_option("c", ["a", "b"]) = NONE
val all_except_option_test3 = all_except_option("b", ["a", "b", "c"]) = SOME ["a", "c"]
val all_except_option_test4 = all_except_option("a", ["a"]) = SOME []

(* 1b. Write a function get_substitutions1, which takes a string list list (a list of list of strings, the
substitutions) and a string s and returns a string list. The result has all the strings that are in
some list in substitutions that also has s, but s itself should not be in the result. Example:
get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred")
(* answer: ["Fredrick","Freddie","F"] *)
Assume each list in substitutions has no repeats. The result will have repeats if s and another string are
both in more than one list in substitutions. Example:
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff")
(* answer: ["Jeffrey","Geoff","Jeffrey"] *)
Use part (a) and ML’s list-append (@) but no other helper functions. Sample solution is around 6 lines. *)

fun get_substitutions1(los, s) =
    case los of
        [] => []
      | x::xs' =>
        case all_except_option(s, x) of
            NONE => get_substitutions1(xs', s)
          | SOME ys' => ys' @ get_substitutions1(xs', s)

val get_substitutions1_test1 = get_substitutions1([], "Jeff") = []
val get_substitutions1_test2 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred") = ["Fredrick", "Freddie", "F"]
val get_substitutions1_test3 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff") = ["Jeffrey", "Geoff", "Jeffrey"]