fun is_older (d1 : int*int*int, d2 : int*int*int) =
    if #1 d1 < #1 d2
    then true
    else if #1 d1 > #1 d2
    then false
    else if #2 d1 < #2 d2
    then true
    else if #2 d1 > #2 d2
    then false
    else if #3 d1 < #3 d2
    then true
    else if #3 d1 > #3 d2
    then false
    else false

fun number_in_month (dates : (int*int*int) list, m : int) =
    if null dates
    then 0
    else let val num = number_in_month(tl dates, m)
	 in if  #2 (hd dates) =  m
	    then 1 + num
	    else num
	 end
	     
fun number_in_months (dates, months) =
    if null dates orelse null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else let val tl_ans = dates_in_month(tl dates, month)
	 in if #2 (hd dates) = month
	    then hd dates :: tl_ans
	    else tl_ans
	 end

fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null dates orelse null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
	
fun get_nth (xs : string list, loc : int) =
    if null xs
    then ""
    else if loc = 1
    then hd xs
    else get_nth(tl xs, loc - 1)
		
fun date_to_string (date : int*int*int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, xs : int list) =
    if null xs orelse hd xs > sum
    then ~1
    else 1 +  number_before_reaching_sum(sum - (hd xs), tl xs)

fun what_month (day : int) =
    if day > 365 orelse day < 1
    then ~1
    else let val months = [32, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]			      
	 in 2 + number_before_reaching_sum(day, months)
	 end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)
(*					
fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else if null (tl dates)
    then SOME(hd dates)
    else let val tl_ans = oldest(tl dates)
	 in if is_older(hd dates, valOf(tl_ans))
	    then SOME(hd dates)
	    else tl_ans
	 end
*)
					
fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else let fun helper (dates : (int*int*int) list) =
		 if null (tl dates)
		 then hd dates
		 else let val tl_ans = helper(tl dates)
		      in if is_older(hd dates, tl_ans)
			 then hd dates
			 else tl_ans
		      end
	 in SOME(helper(dates))
	 end
