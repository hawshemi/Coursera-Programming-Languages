(* Assignment 1 *)


fun is_older(date1: int*int*int, date2: int*int*int) =
    if #1 date1 < #1 date2
    then true
    else if #1 date1 > #1 date2
    then false (*the option that remains is that they are the same*)
    else if #2 date1 < #2 date2
    then true
    else if #2 date1 > #2 date2
    then false
    else if #3 date1 < #3 date2
    then true
    else false


fun number_in_month(list_dates: (int*int*int) list, month: int) =
    if null list_dates
    then 0
    else
        let (* fine to assume argument nonempty because it is local *)
        	fun number_in_month_nonempty(list_dates: (int*int*int) list) =
                if null list_dates
                then 0
                else
                let val tl_ans = number_in_month_nonempty(tl list_dates)
                in
                    if #2(hd list_dates) = month
                    then 1 + tl_ans
                    else 0 + tl_ans
                end
    	in
    	    number_in_month_nonempty list_dates
    	end


fun number_in_months(list_dates: (int*int*int) list, list_months: int list) =
    if null list_dates orelse null list_months
    then 0
    else
        let
            fun number_in_months_nonempty(list_months: int list) =
                if null list_months
                then 0
                else number_in_month(list_dates, hd list_months) +
                    number_in_months_nonempty(tl list_months)
        in
            number_in_months_nonempty list_months
        end


fun dates_in_month(list_dates: (int*int*int) list, month: int) =
    if null list_dates
    then []
    else
        let
            fun dates_in_month_nonempty(list_dates: (int*int*int) list) =
                if null list_dates
                then []
                else
                let val tl_ans = dates_in_month_nonempty(tl list_dates)
                in
                    if #2(hd list_dates) = month
                    then hd list_dates :: tl_ans
                    else tl_ans
                end
        in
            dates_in_month_nonempty list_dates
        end


fun dates_in_months(list_dates: (int*int*int) list, list_months: int list) =
    if null list_dates orelse null list_months
    then []
    else
        let
            fun dates_in_months_nonempty(list_months: int list) =
                if null list_months
                then []
                else dates_in_month(list_dates, hd list_months) @
                    dates_in_months_nonempty(tl list_months)
        in
            dates_in_months_nonempty list_months
        end


fun get_nth(list_strings: string list, n: int) =
    if null list_strings
    then ""
    else if n = 1
    then hd list_strings
    else get_nth(tl list_strings, n - 1)


fun date_to_string(date: int*int*int) =
    let val list_months = ["January", "February", "March", "April",
        "May", "June", "July", "August", "September", "October",
        "November", "December"]
    in
        get_nth(list_months, #2 date) ^ " " ^
            Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end


fun number_before_reaching_sum(sum: int, list_numbers: int list) =
    if sum - hd(list_numbers) > 0
    then 1 + number_before_reaching_sum(sum - hd list_numbers, tl list_numbers)
    else 0

fun what_month(day: int) =
    let val list_days_months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum(day, list_days_months) + 1
    end


fun month_range(day1, day2) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)


fun oldest(list_dates: (int*int*int) list) =
    if null list_dates
    then NONE
    else if null (tl list_dates)
    then SOME(hd list_dates)
    else
        let val val_ans = oldest(tl list_dates)
        in
            if is_older(hd list_dates, valOf val_ans)
            then SOME(hd list_dates)
            else val_ans
        end
