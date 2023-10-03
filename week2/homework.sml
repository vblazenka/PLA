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