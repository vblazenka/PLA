(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

(* 
 * put your solutions for problem 1 here 
 *)

(* (a) Write a function all_except_option, which takes a string and a string list. Return NONE if thestring is not in the list,
   else return SOME lst where lst is identical to the argument list except the stringis not in it. You may assume the string is 
   in the list at most once. Use same_string, provided to you,to compare strings. Sample solution is around 8 lines. *)
fun all_except_option(str, []) = NONE
  | all_except_option(str, x::xs) =
      if same_string(str, x) then
         SOME xs
      else
        case all_except_option(str, xs) of
          NONE => NONE
        | SOME rest => SOME (x :: rest);

(* val test1__0 = all_except_option ("string", ["string"]) = SOME []
val test1__1 = all_except_option ("string", []) = NONE
val test1_1_0 = all_except_option ("string", ["string", "cat", "dog"]) = SOME ["cat", "dog"]
val test1__2 = all_except_option ("string", ["strin"])  = NONE
val test1__3 = all_except_option ("string", ["strin", "strin99"]) = NONE
val test1__4 = all_except_option ("string", ["strin", "strin99", "string"]) = SOME ["strin","strin99"] *)

(* (b) Write a function get_substitutions1, which takes a string list list (a list of list of strings, thesubstitutions)
   and a string s and returns a string list. The result has all the strings that are insome list in substitutions that also has s, 
   but s itself should not be in the result. Example:get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred")
   (* answer: ["Fredrick","Freddie","F"] *)Assume each list in substitutions has no repeats.
   The result will have repeats if s and another string areboth in more than one list in substitutions.
   Example:get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff")
   (* answer: ["Jeffrey","Geoff","Jeffrey"] *)  *)
fun get_substitutions1([], str) = []
  | get_substitutions1(xs::xss, str) =
      case all_except_option(str, xs) of
           NONE => get_substitutions1(xss, str)
         | SOME rest => rest @ get_substitutions1(xss, str)

(* val test2__0 = get_substitutions1([["foo"],["there"]], "foo") = []
val test2_01 = get_substitutions1([["B", "A", "C"], ["G", "F"], ["X", "A", "Z"]], "A") = ["B", "C", "X", "Z"]
val test2__1 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
val test2__2 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"] *)

(* (c) Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursivelocal helper function. 
*)

fun get_substitutions2(xss, str) =
   let
     fun inner_fn([], acc) = acc
       | inner_fn(xs::xss, acc) =
         case all_except_option(str, xs) of
            NONE => inner_fn(xss, acc)
            | SOME rest => inner_fn(xss, acc @ rest)
   in
     inner_fn(xss, [])
   end;

(* val test2__0 = get_substitutions2([["foo"],["there"]], "foo") = []
val test2_01 = get_substitutions2([["B", "A", "C"], ["G", "F"], ["X", "A", "Z"]], "A") = ["B", "C", "X", "Z"]
val test2__1 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
val test2__2 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"] *)

(* (d) Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and(c)) and a full name 
of type {first:string,middle:string,last:string} and returns a list of fullnames (type {first:string,middle:string,last:string} list). 
The result is all the full names youcan produce by substituting for the first name (and only the first name) 
using substitutions and parts (b)or (c). The answer should begin with the original name (then have 0 or more other names). 
Example:similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],{first="Fred", middle="W", last="Smith"}) 
answer: [{first="Fred", last="Smith", middle="W"},
         {first="Fredrick", last="Smith", middle="W"},
         {first="Freddie", last="Smith", middle="W"},
         {first="F", last="Smith", middle="W"}] *)
fun similar_names(xss, {first=f, middle=m, last=l}) =
   case get_substitutions1(xss, f) of
      [] => []
    | rest =>
         let
            fun inner_fn [] = []
              | inner_fn (x::xs) =
                  {first=x, middle=m, last=l} :: inner_fn(xs)
         in
            { first=f, middle=m, last=l } :: inner_fn(rest)
         end

val test4___0 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Vedran", middle="W", last="Smith"}) = []
val test4___ = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
        
(* =================================================================================== *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

exception NoCardInDeck

(* put your solutions for problem 2 here *)


(*  Write a function card_color, which takes a card and returns its color (spades and clubs are black,
 diamonds and hearts are red). Note: One case-expression is enough. *)
fun card_color(c : card) =
   case c of
        (Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red

(* Write a function card_value, which takes a card and returns its value (numbered cards have their
 number as the value, aces are 11, everything else is 10). Note: One case-expression is enough. *)
 fun card_value(c : card) =
   case c of
     (_, Ace) => 11
     | (_, Num n) => n
     | _ => 10


(* Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
 list that has all the elements of cs except c. If c is in the list more than once, remove only the last one.
 If c is not in the list, raise the exception e. You can compare cards with = *)

fun is_same(c1 : card, c2 : card) =
   c1 = c2

fun remove_card (cs : card list, c : card, e : exn) =
  let
    fun inner_fn [] = NONE
      | inner_fn (x::xs) =
          case inner_fn xs of
              SOME new_tail => SOME (x :: new_tail)
            | NONE =>
                if x = c
                then SOME xs
                else NONE
  in
    case inner_fn cs of
        SOME result => result
      | NONE => raise e
  end


(* val card1 = (Hearts, King)
val test1 = remove_card([(Spades, Queen), card1], card1, NoCardInDeck) = [(Spades, Queen)]
val test2 = remove_card([(Spades, Queen), (Diamonds, Num 8)], card1, NoCardInDeck) = [(Spades, Queen), (Diamonds, Num 8)]
val test3 = remove_card([(Spades, Queen), card1, (Hearts, Ace), card1, (Diamonds, Num 8)], card1, NoCardInDeck) =
   [(Spades, Queen), card1, (Hearts, Ace), (Diamonds, Num 8)] *)


(*  Write a function all_same_color, which takes a list of cards and returns true if all the cards in the
 list are the same color. Hint: An elegant solution is very similar to one of the functions using nested
 pattern-matching in the lectures. *)

 fun all_same_color [] = false
   | all_same_color (x::xs) =
   let
     fun same([]) = true
       | same(c::cs) =
          if card_color(x) = card_color(c)
          then same(cs)
          else
            false
   in
    same(xs)
   end

(* Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally
 de ned helper function that is tail recursive. (Take calls use a constant amount of stack space as a
 requirement for this problem.) *)

fun sum_cards [] = 0
  | sum_cards(xs: card list) =
    let
      fun helper([], acc) = acc
        | helper(c::cs, acc ) =
            helper(cs, acc + card_value(c))
    in
      helper(xs, 0)
    end;


 (*
  A game is played with a card-list and a goal. The player has a list of held-cards, initially empty. The player
 makes a move by either drawing, which means removing the rst card in the card-list from the card-list and
 adding it to the held-cards, or discarding, which means choosing one of the held-cards to remove. The game
 ends either when the player chooses to make no more moves or when the sum of the values of the held-cards
 is greater than the goal.
 The objective is to end the game with a low score (0 is best). Scoring works as follows: Let sum be the sum
 of the values of the held-cards. If sum is greater than goal, the preliminary score is three times (sum goal),
 else the preliminary score is (goal sum). The score is the preliminary score unless all the held-cards are
 the same color, in which case the score is the preliminary score divided by 2 (and rounded down as usual
 with integer division; use MLs div operator).
 *)
(* Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes
 the score as described above. *)
fun score(held_cards: card list, g : int) =
  let
    val total_sum = sum_cards(held_cards)
    val preliminary_score = 
      if total_sum > g
      then 3 * (total_sum - g)
      else
        g - total_sum
  in
    case (all_same_color(held_cards) orelse (held_cards = [])) of
      true => preliminary_score div 2
    | false => preliminary_score
  end;

(*
 Write a function officiate, which runs a game. It takes a card list (the card-list) a move list
 (what the player does at each point), and an int (the goal) and returns the score at the end of the
 game after processing (some or all of) the moves in the move list in order. Use a locally de ned recursive
 helper function that takes several arguments that together represent the current state of the game. As
 described above:
 The game starts with the held-cards being the empty list.
 The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
 If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
 not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
 exception.
 If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes
 the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues
 with a larger held-cards and a smaller card-list
*)
fun officiate (card_list : card list, move_list : move list, goal : int) =
  let
    fun inner_fn(held_cards: card list, remaining_deck: card list, moves: move list) =
      case moves of
          [] => score(held_cards, goal)
        | (Discard c) :: ms =>
            let
              val new_held_cards = remove_card(held_cards, c, IllegalMove)
            in
              inner_fn(new_held_cards, remaining_deck, ms)
            end
        | Draw :: ms =>
            case remaining_deck of
                [] => score(held_cards, goal)
              | c::cs =>
                  let
                    val new_held_cards = c :: held_cards
                    val new_sum = sum_cards(new_held_cards)
                  in
                    if new_sum > goal
                    then score(new_held_cards, goal)
                    else inner_fn(new_held_cards, cs, ms)
                  end
  in
    inner_fn([], card_list, move_list)
  end;
