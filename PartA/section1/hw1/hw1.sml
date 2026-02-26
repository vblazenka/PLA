(* Homework1 answer
 * Author: Vedran B *)

(* 1. Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
 the rst argument is a date that comes before the second argument. (If the two dates are the same,
 the result is false.) *)
fun is_older(d1: int * int * int, d2 : int * int * int) =
    let
        val (x1, x2, x3) = d1
        val (y1, y2, y3) = d2
    in
        if x1 < y1 then true
        else if x1 > y1 then false
        else
            if x2 < y2 then true
            else if x2 > y2 then false
            else
             x3 < y3
    end;

val test1_0 = is_older ((1,2,3),(2,3,4)) = true
val test1_1 = is_older ((1,2,3),(1,2,4)) = true
val test1_2 = is_older ((1,2,3),(1,3,3)) = true
val test1_3 = is_older ((1,2,3),(2,2,3)) = true
val test1_4 = is_older ((1,2,3),(1,2,3)) = false
val test1_5 = is_older ((11,11,3),(12,12,2)) = true
val test1_6 = is_older ((5,4,4),(4,5,4)) = false
val test1_7 = is_older ((1,2,25),(6,7,8)) = true

(* 2. Function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month. *)
fun number_in_month(dates: (int*int*int) list, month: int) =
    if null dates then 0
    else
      let
        val curr_month = (#2 (hd dates))
      in
        if (curr_month = month)
        then 1 + number_in_month(tl dates, month)
        else number_in_month(tl dates, month)
      end;

(* val test2_0 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2_1 = number_in_month ([(2012,2,28),(2013,12,1)],3) = 0
val test2_2 = number_in_month ([(2012,3,28),(2013,3,1)],3) = 2
val test2_3 = number_in_month ([(2012,2,28)],2) = 1
val test2_4 = number_in_month ([(2012,2,28)],5) = 0
val test2_5 = number_in_month ([],5) = 0 *)

(* 3. Function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. *)
fun number_in_months(dates: (int*int*int) list, months: int list) = 
    if null dates then 0
    else
        let
            fun inner_fn(months: int list, date: (int * int * int)) =
                if null months then false
                else
                  if (hd months) = (#2 date)
                  then true
                  else inner_fn(tl months, date)
        in
            if inner_fn(months, hd dates)
            then 1 + number_in_months(tl dates, months)
            else number_in_months(tl dates, months)
        end;

(* val test3_0 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3_1 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3]) = 2
val test3_2 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,11]) = 2
val test3_3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,12]) = 4
val test3_4 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[15,13,14]) = 0
val test3_5 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[12,13,14]) = 1
val test3_6 = number_in_months ([],[2,3,4]) = 0
val test3_7 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0 *)

 (* 4. Function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given. *)
fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates then []
    else
        if ((#2 (hd dates)) = month)
        then (hd dates) :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

(* val test4_0 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4_1 = dates_in_month ([(2012,2,28),(2013,2,1)],2) = [(2012,2,28),(2013,2,1)]
val test4_2 = dates_in_month ([],2) = []
val test4_3 = dates_in_month ([(2012,2,28),(2013,12,1)],0) = []
val test4_4 = dates_in_month ([(2013,12,1),(2012,2,28)],2) = [(2012,2,28)]
val test4_5 = dates_in_month ([(2013,11,1),(2012,2,28),(2013,11,28),(2011,11,12)],11) = [(2013,11,1),(2013,11,28),(2011,11,12)] *)

(* 5. Function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. *)
fun dates_in_months(dates: (int*int*int) list, months: int list) = 
    if null dates then []
    else
        let
            fun inner_fn(date:int*int*int, months: int list) =
                if null months
                then false
                else
                    if (hd months) = (#2 date)
                    then true
                    else inner_fn(date, tl months)
        in
            if inner_fn (hd dates, months)
            then (hd dates) :: dates_in_months(tl dates, months)
            else dates_in_months(tl dates, months)
        end;

(* 2nd version that uses previousky defined dates_in_month function *)
(* fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else
        dates_in_month(dates, hd months) @ dates_in_months(dates, tl months) *)

(* val test5_0 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test5_1 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = [] *)

(* 6. Function get_nth that takes a list of strings and an int n and returns the nth element of the list 
where the head of the list is 1st. *)
fun get_nth(strings: string list, n: int) =
    if null strings then ""
    else
        if n = 1
        then hd strings
        else
            get_nth(tl strings, n - 1) 


(* val test6_0 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6_1 = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"
val test6_2 = get_nth (["hi", "there", "how", "are", "you"], 0) = ""
val test6_3 = get_nth ([], 2) = "" *)
    
(* 7. Function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December. *)
fun date_to_string (yr: int, month:int, day: int) =
    let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, month) ^ " " ^ Int.toString(day) ^  ", " ^ Int.toString(yr) 
    end;

(* val test7 = date_to_string (2013, 6, 1) = "June 1, 2013" *)

(* 8. Function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. *)
fun number_before_reaching_sum(sum: int, sums: int list) =
    let
      fun inner_fn(sum: int, sums: int list, counter: int) =
        if null sums then counter
        else
          if (hd sums >= sum) then counter
          else
            inner_fn(sum - (hd sums), tl sums, counter + 1)
    in
        inner_fn(sum, sums, 0)
    end;

(* 
val test8_0 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test8_1 = number_before_reaching_sum (0, [1,2,3,4,5]) = 0
val test8_2 = number_before_reaching_sum (2, [1,2,3,4,5]) = 1
val test8_3 = number_before_reaching_sum (10, []) = 0
val test8_4 = number_before_reaching_sum (5, [3,1,2]) = 2
val test8_5 = number_before_reaching_sum (5, [3,2,2]) = 1
val test8_6 = number_before_reaching_sum (6, [4,1,1,1]) = 2
val test8_7 = number_before_reaching_sum (1, [2]) = 0 *)

 (* 9. Function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). *)
fun what_month(dayOfAYear: int) = 
    if dayOfAYear = 0 then 0
    else
     let
          val m = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

          fun sum_until(months, sum, doy, c) =
                if null months
                then 0
                else
                     if (sum + hd months >= doy)
                     then c + 1
                     else
                          sum_until(tl months, sum + hd months, doy, c + 1)
     in
        sum_until(m, 0, dayOfAYear, 0)
     end;

val test9_0 = what_month 70 = 3
val test9_1 = what_month 0 = 0
val test9_2 = what_month 1 = 1
val test9_3 = what_month 60 = 3
val test9_4 = what_month 365 = 12
val test9_5 = what_month 85 = 3
val test9_6 = what_month 145 = 5

(* 10. Function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. *)
fun month_range(day1: int, day2: int) =
    let
        val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

        fun generate_range(current: int, end_: int, months: int list) =
            if current > end_ then []
            else
                let
                    val month = what_month(current)
                in
                    month :: generate_range(current + 1, end_, months)
                end
    in
        generate_range(day1, day2, days_in_months)
    end

val test10_0 = month_range (31, 34)  = [1,2,2,2]
val test10_1 = month_range (1,365)   = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12]
val test10_2 = month_range (335,365) = [12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12]
val test10_3 = month_range (85,145)  = [3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5]
val test10_4 = length(month_range (1,365)) = length([1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12])
val test10_5 = length(month_range (335,365)) = length([12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12])

(* 11. Function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)
fun oldest(dates: (int*int*int) list) = 
    let
        fun find_oldest(dates, current_oldest) =
            if null dates then current_oldest
            else
                let
                    val hd = hd dates
                    val tl = tl dates
                in
                    if current_oldest = NONE then find_oldest(tl, SOME hd)
                    else
                        let
                            val SOME old = current_oldest
                        in
                            if is_older(hd, old) then find_oldest(tl, SOME hd)
                            else find_oldest(tl, SOME old)
                        end
                end
    in
        find_oldest(dates, NONE)
    end;
        
    
val test11_0 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11_1 = oldest([]) = NONE
val test11_2 = oldest([(2012,2,28),(2011,4,28),(2011,3,31)]) = SOME (2011,3,31)
val test11_3 = oldest([(2011,4,28),(2012,2,28),(2011,3,31)]) = SOME (2011,3,31)
val test11_4 = oldest([(1,1,1),(1,1,1),(1,1,1)]) = NONE
val test11_5 = oldest([(1,2,3),(5,2,3),(7,2,3),(3,2,3)]) = SOME (1,2,3)
val test11_6 = oldest([(5,5,2),(5,10,2),(5,2,2),(5,12,2)]) = SOME (5,2,2)
val test11_7 = oldest([(5,12,15),(5,12,10),(5,12,1)]) = SOME (5,12,1)