(* Author: Vedran B *)
(* Coursera Programming Languages, Homework 3 *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals(xs: string list) =
    List.filter (Char.isUpper o String.sub o (fn x => (x, 0))) xs

fun longest_string1 [] = ""
  | longest_string1 xs =
        foldl (fn(x, acc) => if String.size(x) > String.size(acc) then x else acc) "" xs

fun longest_string2 [] = ""
  | longest_string2 xs =
    foldl (fn(x, acc) => if String.size(x) >= String.size(acc) then x else acc) "" xs

fun longest_string_helper cmp xs = 
    foldl (fn(x, acc) => if cmp(String.size(x), String.size(acc)) then x else acc) "" xs

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string3 o only_capitals

fun rev_string s =  String.implode (rev (String.explode s))

fun first_answer f [] = raise NoAnswer
    | first_answer f (x::xs) =
            case f x of
                    SOME result => result
                | NONE => first_answer f xs

fun all_answers f xs =
    let
        fun helper ([], acc) = SOME (rev acc)
          | helper (x::xs, acc) =
                case f x of
                    NONE => NONE
                  | SOME lst => helper(xs, lst @ acc)
    in
        helper(xs, [])
    end

fun count_wildcards p =
    g (fn () => 1) (fn _ => 0) p


fun count_some_var (s, p) =
    g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
    let
        fun vars Wildcard = []
          | vars (Variable x) = [x]
          | vars UnitP = []
          | vars (ConstP _) = []
          | vars (TupleP ps) = List.foldl (fn (p, acc) => vars p @ acc) [] ps
          | vars (ConstructorP(_, p)) = vars p

        fun has_duplicates [] = false
          | has_duplicates (x::xs) = List.exists (fn y => x = y) xs orelse has_duplicates xs
    in
        not (has_duplicates (vars p))
    end

fun match (v, p) =
    case (v, p) of
        (_, Wildcard) => SOME []
      | (_, Variable x) => SOME [(x, v)]
      | (Unit, UnitP) => SOME []
      | (Const n, ConstP m) => if n = m then SOME [] else NONE
      | (Tuple vs, TupleP ps) =>
            if length vs = length ps then
                all_answers match (ListPair.zip (vs, ps))
            else NONE
      | (Constructor(s1, v1), ConstructorP(s2, p1)) =>
            if s1 = s2 then match (v1, p1) else NONE
      | _ => NONE

fun first_match v ps =
    first_answer (fn p => match (v, p)) ps
    handle NoAnswer => NONE
