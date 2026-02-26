(* Programming Languages, Dan Grossman *)
(* Section 3: Map and Filter *)

fun map(f, xs) =
    case xs of
        []     => []
      | x::xs' => (f x)::map(f, xs')

val x1 = map((fn x => x + 1), [4, 8, 12, 16])
val x2 = map(hd, [[1, 2, 3], [4, 5, 6]])

fun filter(f, xs) =
    case xs of
         [] => []
      | x::xs' => if (f x) then x :: filter(f, xs') else filter(f, xs')

fun is_even n = n mod 2 = 0

val x3 = filter(is_even, [1, 2, 3, 4, 5, 6, 7, 8, 9])