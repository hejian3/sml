
fun is_older (date1 : (int * int * int), date2 : (int * int * int)) =
    if (#1 date1) <> (#1 date2)
    then (#1 date1) < (#2 date2)
    else if (#2 date1) <> (#2 date2)
    then (#2 date1) < (#2 date2)
    else (#3 date1) < (#3 date2)
	
fun number_in_month (dates : (int * int * int) list , month : int) =
    if null dates
    then 0
    else
        let val date = hd(dates);
        in
            if (#2 date) = month
            then 1 + number_in_month(tl(dates),month)
            else 0 + number_in_month(tl(dates),month)
        end 

fun number_in_months (date : (int * int * int) list , months : int list) =
    if null months
    then 0
    else
        number_in_month(date,hd(months)) + number_in_months(date, tl(months))

fun dates_in_month (dates : (int * int * int) list , month : int) =
    if null dates
    then []
    else
        let val date = hd(dates)
        in
            if (#2 date) = month
            then date :: dates_in_month(tl(dates),month)
            else dates_in_month(tl(dates),month)
        end

fun dates_in_months (dates : (int * int * int) list , months : int list) =
    if null months
    then []
    else
        let val l = dates_in_month(dates,hd(months))
        in
            if null l
            then dates_in_months(dates,tl(months))
            else l @ dates_in_months(dates,tl(months))
        end 

fun get_nth (strs : string list , n : int) =
    if n = 1
    then hd(strs)
    else get_nth(tl(strs),n-1)

fun date_to_string (date : (int * int * int)) =
    let val months = ["January", "February", "March", "April",
		  "May", "June", "July", "August", "September", "October", "November", "December"]
    in	
	    get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int ,nums : int list) =
    let fun nums_sum (count : int,total : int , nums : int list) =
	if total >= sum orelse total + hd(nums) >= sum
	then count
	else nums_sum(count+1,total + hd(nums),tl(nums))
    in
	    nums_sum(0,0,nums)
    end
	
fun what_month (day:int) =
    let val days = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	    1 + number_before_reaching_sum(day,days)
    end

fun month_range (day1 : int , day2 :int) =
    if day1 > day2
    then []
    else
	    what_month(day1)::month_range(day1 + 1,day2)

fun oldest (date : (int * int * int) list) =
    if null date
    then NONE
    else
	let fun older(date1 : (int * int * int) , dates : (int * int * int) list) =
	    if null dates
	    then SOME(date1)
	    else
            if is_older (date1,hd(dates))
            then older(date1,tl(dates))
            else older(hd(dates),tl(dates))
	in
	    older(hd(date),tl(date))
	end

