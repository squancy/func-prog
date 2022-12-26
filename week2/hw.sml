(* #1 *)
fun is_older ((y1 : int, m1 : int, d1), (y2 : int, m2 : int, d2 : int)) =
  if y1 = y2 andalso m1 = m2 andalso d1 = d2
  then false
  else if y2 > y1
  then true
  else if y2 < y1
  then false
  else if m2 > m1
  then true
  else if m2 < m1
  then false
  else if d2 > d1
  then true
  else false

(* #2 *)
fun number_in_month (ls : (int * int * int) list, month : int) =
  if null ls
  then 0
  else if (#2 (hd ls)) = month
  then 1 + number_in_month (tl ls, month)
  else number_in_month (tl ls, month)

(* #3 *)
fun number_in_months (ls : (int * int * int) list, months: int list) =
  let
    fun iterate_over_months (months : int list) =
      if null months
      then 0
      else number_in_month (ls, hd months) + iterate_over_months (tl months)
  in
    iterate_over_months (months) 
  end

(* #4 *)
fun dates_in_month (ls : (int * int * int) list, month : int) =
  let
    fun iterate_over_dates (ls : (int * int * int) list) =
      if null ls
      then []
      else if #2 (hd ls) = month
      then (hd ls) :: iterate_over_dates (tl ls)
      else iterate_over_dates (tl ls)
  in
    iterate_over_dates (ls) 
  end

(* #5 *)
fun dates_in_months (ls : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month (ls, hd months) @ dates_in_months (ls, tl months)

(* #6 *)
fun get_nth (ls : string list, n : int) =
  let
    fun until_n (ls : string list, n : int, acc : int) =
      if n = acc
      then hd ls
      else until_n (tl ls, n, acc + 1)
  in
    until_n (ls, n, 1)
  end

(* #7 *)
fun date_to_string (date : (int * int * int)) =
  let
    val month_names = [
      "January", "February", "March", "April", "May", "June", "July",
      "August", "September", "October", "November", "December"
    ]
    val year = Int.toString (#1 date)
    val month = get_nth (month_names, #2 date)
    val day = Int.toString (#3 date)
  in
    month ^ " " ^ day ^ ", " ^ year
  end

(* #8 *)
fun number_before_reaching_sum (sum : int, ls : int list) =
  let
    fun until_sum (sum : int, ls : int list, acc : int, pos : int) =
      if acc >= sum
      then pos - 1
      else until_sum (sum, tl ls, acc + hd ls, pos + 1)
  in
    until_sum (sum, ls, 0, 0)
  end

(* #9 *)
fun what_month (day : int) =
  let
    val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    val month_number = number_before_reaching_sum (day, month_days) + 1
  in
    month_number
  end

(* #10 *)
fun month_range (day1 : int, day2 : int) =
  let
    fun month_list (cday : int) =
      if cday > day2
      then []
      else what_month (cday) :: month_list (cday + 1)
  in
    month_list (day1)
  end

(* #11 *)
fun oldest (ls : (int * int * int) list) =
  let
    fun find_oldest (ls : (int * int * int) list, coldest : (int * int * int)) =
      if null ls
      then coldest
      else if is_older (coldest, hd ls)
      then find_oldest (tl ls, hd ls)
      else find_oldest (tl ls, coldest)
  in
    if null ls
    then NONE
    else SOME (find_oldest (ls, hd ls))
  end

(* #12.1 *)
fun remove_duplicates (months : int list) =
  let
    fun is_month_in_ls (months : int list, month: int) =
      if null months
      then false
      else if hd months = month
      then true
      else is_month_in_ls (tl months, month)

    fun check_months (months : int list, cmonth : int) =
      if cmonth > 12
      then []
      else if is_month_in_ls (months, cmonth)
      then cmonth :: check_months (months, cmonth + 1)
      else check_months (months, cmonth + 1)
  in
    check_months (months, 1)
  end

fun number_in_months_challenge (ls : (int * int * int) list, months: int list) =
  number_in_months (ls, remove_duplicates (months))

(* #12.2 *)
fun dates_in_months_challenge (ls : (int * int * int) list, months: int list) =
  dates_in_months (ls, remove_duplicates (months))

(* #13 *)
fun reasonable_date ((y : int, m : int, d : int)) =
  let
    fun is_leap_year (y : int) =
      if (y mod 400 = 0 orelse y mod 4 = 0) andalso y mod 100 <> 0
      then true
      else false

    fun nth_element (ls : int list, n : int) =
      let
        fun until_n (ls : int list, n : int, acc : int) =
          if n = acc
          then hd ls
          else until_n (tl ls, n, acc + 1)
      in
        until_n (ls, n, 1)
      end

    fun check_d_for_m (leap : bool) =
      let
        val leap_months = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val non_leap_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      in
        if leap
        then d <= nth_element (leap_months, m)
        else d <= nth_element (non_leap_months, m)
      end
  in
      if y < 1 orelse m < 1 orelse m > 12 orelse d < 1
      then false
      else if is_leap_year (y)
      then check_d_for_m (true)
      else check_d_for_m (false)
  end
