(* 1. Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.) *)
fun is_older(date1 : int * int * int, date2 : int * int * int) : bool =
    if #1 date1 < #1 date2 orelse
       #2 date1 < #2 date2 orelse
       #3 date1 < #3 date2
    then
        true
    else
        false

val is_older_test1 = is_older((2023, 4, 15), (2023, 3, 10)) = false
val is_older_test2 = is_older((2023, 2, 15), (2023, 3, 10)) = true
val is_older_test3 = is_older((2023, 2, 09), (2023, 2, 10)) = true

(* 2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month. *)
fun number_in_month([], _) = 0 |
    number_in_month(dates : (int * int * int) list, month : int) : int =
        if #2 (hd dates) = month then
            1 + number_in_month(tl dates, month)
        else
            number_in_month(tl dates, month)

val number_in_month_test1 = number_in_month([(2023, 4, 15), (2021, 8, 12)], 5) = 0
val number_in_month_test2 = number_in_month([(2023, 4, 15), (2021, 8, 12)], 8) = 1
val number_in_month_test3 = number_in_month([(2023, 4, 15), (2021, 8, 12), (2019, 4, 27)], 4) = 2

(* 3.  Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)
fun number_in_months([], _) = 0 |
    number_in_months(_, [])  = 0 |
    number_in_months(dates : (int * int * int) list, months : int list) : int =
        number_in_month(dates, hd months) + number_in_months(dates, tl months)
    

val number_in_months_test1 = number_in_months([(2023, 4, 15), (2021, 8, 12)], [1, 4]) = 1
val number_in_months_test2 = number_in_months([(2023, 4, 15), (2021, 8, 12)], [1, 5, 6, 7]) = 0
val number_in_months_test3 = number_in_months([(2023, 4, 15), (2021, 8, 12)], [8, 4]) = 2

(* 4. Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given. *)
fun dates_in_month([], _) = [] |
    dates_in_month(dates: (int * int * int) list, month : int) : (int * int * int) list =
        if #2 (hd dates) = month then
            hd dates :: dates_in_month(tl dates, month)
        else
            dates_in_month(tl dates, month)

val dates_in_month_test1 = dates_in_month([], 1) = []
val dates_in_month_test2 = dates_in_month([(2023, 4, 15), (2021, 8, 12), (2019, 4, 27)], 4) = [(2023, 4, 15), (2019, 4, 27)]
val dates_in_month_test3 = dates_in_month([(2023, 4, 15), (2021, 8, 12), (2019, 4, 27)], 8) = [(2021, 8, 12)]

(* 5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML’s list-append operator (@). *)
fun dates_in_months([], _) = [] |
    dates_in_months(_, []) = [] |
    dates_in_months(dates : (int * int * int) list, months : int list) : (int * int * int) list =
        dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
    

val dates_in_months_test1 = dates_in_months([], [1]) = []
val dates_in_months_test2 = dates_in_months([(2023, 4, 15), (2021, 8, 12), (2019, 4, 27)], [4, 8]) = [(2023, 4, 15), (2019, 4, 27), (2021, 8, 12)]
val dates_in_months_test3 = dates_in_months([(2023, 4, 15), (2021, 8, 12), (2019, 4, 27)], [8]) = [(2021, 8, 12)]

(* 6. Write a function get_nth that takes a list of strings and an int n and returns the n
th element of the list where the head of the list is 1st. Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay *)

fun get_nth([], _) = "" |
    get_nth(strings : string list, n : int) : string =
        if n = 1 then
            hd strings
        else
            get_nth(tl strings, n - 1)
    

val get_nth_test1 = get_nth(["a", "b", "c", "d"], 2) = "b"
val get_nth_test2 = get_nth(["a", "b", "c", "d"], 7) = ""

(* 7. Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December *)

fun date_to_string(date : int * int * int) : string =
    let
        val (y, m, d) = date
        val month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(month_names, m) ^ " " ^ Int.toString(d) ^ ", " ^ Int.toString(y)
    end

 val date_to_string_test1 = date_to_string((2021, 3, 14)) = "March 14, 2021"

(* 8. Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case. *)

fun number_before_reaching_sum(0, _)  = 0 |
    number_before_reaching_sum(_, []) = 0 |
    number_before_reaching_sum(sum : int, nums : int list) : int =
        let 
            val remaining_sum = sum - hd (tl nums)
        in
            if remaining_sum <= 1 then
                hd nums
            else
                number_before_reaching_sum(remaining_sum, tl nums)
        end

val number_before_reaching_sum_test1 = number_before_reaching_sum(5, []) = 0;
val number_before_reaching_sum_test2 = number_before_reaching_sum(10, [1, 2, 3, 4, 5]) = 3;
val number_before_reaching_sum_test3 = number_before_reaching_sum(4, [1, 2, 3, 4, 5]) = 2;

(* 9. Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem. *)

fun what_month(day : int) : int =
    let
        val days_in_months = [31, 30, 31, 30, 31, 30, 31, 31, 30, 31, 30]
    in
        number_before_reaching_sum(day, days_in_months) + 1
    end

val what_month_test1 = what_month(35) = 2;
val what_month_test2 = what_month(70) = 3;

(* 10. Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2. *)

fun month_range(day1 : int, day2 : int) : int list =
    if day1 > day2 then 0
    else
        let
            val month = what_month day1
        in
            month :: month_range(day1+1, day2)
        end

val month_range_test1 = month_range(30, 33) = [1, 1, 2, 2]