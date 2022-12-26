(* #1 *)

fun nth_power (b : int, e : int) =
  if e = 0
  then 1
  else b * nth_power (b, e - 1)

fun alternate (ls : int list) =
  let 
    fun list_sum (ls : int list, acc : int) =
      if null ls
      then 0
      else ((hd ls) * (nth_power (~1, acc))) + (list_sum ((tl ls), (acc + 1)))
  in
    list_sum (ls, 0)
  end

val al1 = alternate [5, 5, 5, 5, 5, 5, 5, 5, 5]

(* #2 *)
fun min_max (ls : int list) =
  let
    fun max (ls : int list) =
      if null ls
      then 0
      else if (hd ls) > max (tl ls)
      then (hd ls)
      else max (tl ls)

    fun min (ls : int list) =
      if null ls
      then valOf Int.maxInt
      else if (hd ls) < min (tl ls)
      then (hd ls)
      else min (tl ls)
  in
    (min ls, max ls)
  end

val mm1 = min_max [1, 2, 3, 4, 5, 6, 7, 10000, 8, 9, 10, ~2, 11, 12, 14, 15, 15]

(* #3 *)
fun cumsum (ls : int list) =
  let
    fun sum_until_n (ls : int list) =
      if null ls
      then 0
      else (hd ls) + (sum_until_n (tl ls))

    fun first_n_elements (ls : int list, n : int) =
      if n = 0
      then []
      else (hd ls) :: (first_n_elements ((tl ls), n - 1))

    fun list_length (ls : int list) =
      if null ls
      then 0
      else 1 + (list_length (tl ls))

    fun partial_sums (ls : int list, n : int) =
      if n - 1 = list_length ls
      then []
      else sum_until_n (first_n_elements (ls, n)) :: partial_sums (ls, n + 1)
  in
    partial_sums (ls, 1)
  end

val cs1 = cumsum [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

(* #4 *)
fun greeting (name : string option) = 
  if not (isSome name)
  then "Hello there, you!"
  else "Hello there, " ^ valOf name ^ "!"

val g1 = greeting (SOME "Mark")

fun append (ls1 : int list, ls2 : int list) =
  if null ls1
  then ls2
  else (hd ls1) :: append (tl ls1, ls2)

(* #5 *)
fun repeat (ls : int list, ls_r : int list) =
  let
    fun repeat_n_k_times (n : int, k : int) =
      if k = 0
      then []
      else n :: repeat_n_k_times (n, k - 1)

    fun repeat_elements (ls : int list, ls_r : int list) =
      if null ls_r
      then []
      else append (repeat_n_k_times (hd ls, hd ls_r), repeat_elements (tl ls, tl ls_r))
  in
    repeat_elements (ls, ls_r)
  end

val r1 = repeat ([1, 2, 3], [4, 0, 3])       
val r2 = repeat ([3, 4, 5], [1, 2, 3])

(* #6 *)
fun addOpt (n1 : int option, n2 : int option) =
  if isSome n1 andalso isSome n2
  then SOME (valOf n1 + valOf n2)
  else NONE

val ao1 = addOpt (SOME 3, NONE)
val ao2 = addOpt (SOME 5, SOME 2)

(* #7 *)
fun addAllOpt (ls : int option list) =
  let
    fun is_all_none (ls : int option list) =
      if null ls
      then true
      else if not (isSome (hd ls))
      then true andalso is_all_none (tl ls)
      else false

    fun sum_elements (ls : int option list) =
      if null ls
      then 0
      else if not (isSome (hd ls))
      then 0 + (sum_elements (tl ls))
      else (valOf (hd ls)) + (sum_elements (tl ls))

    val result = sum_elements (ls) 
  in
    if is_all_none (ls)
    then NONE
    else SOME result
  end

val aao1 = addAllOpt ([NONE, NONE, NONE])

(* #8 *)
fun any (ls : bool list) =
  if null ls
  then false
  else (hd ls) orelse any (tl ls)

val any1 = any ([false, false, false, false])

(* #9 *)
fun all (ls : bool list) =
  if null ls
  then true
  else (hd ls) andalso any (tl ls)

val all1 = all ([true, true, true, true])

(* #10 *)
fun zip (ls1 : int list, ls2 : int list) =
  if null ls1 orelse null ls2
  then []
  else (hd ls1, hd ls2) :: (zip (tl ls1, tl ls2))

val zip1 = zip ([1, 2, 3], [4, 5, 6, 7, 8, 9])

(* Global helper function *)
fun list_length (ls : int list) =
  if null ls
  then 0
  else 1 + list_length (tl ls)

(* #11 *)
fun zipRecycle (ls1 : int list, ls2 : int list) =
  let
    fun make_same_length (ls : int list, n : int, len : int, acc : int) =
      if n = 0
      then []
      else (List.nth ((ls), (acc mod len))) :: (make_same_length (ls, n - 1, len, acc + 1))

    val len_ls1 = list_length (ls1)
    val len_ls2 = list_length (ls2)
  in
    if len_ls1 < len_ls2
    then zip (make_same_length (ls1, len_ls2, len_ls1, 0), ls2)
    else if list_length (ls1) > list_length (ls2)
    then zip (ls1, make_same_length (ls2, len_ls1, len_ls2, 0))
    else zip (ls1, ls2)
  end

val zipr1 = zipRecycle ([1, 2, 3], [1, 2, 3, 4, 5, 6, 7])
val zipr2 = zipRecycle ([7, 4, 9, 4, 8, 3], [1, 3, 5, 1, 3, 4])

(* #12 *)
fun zipOut (ls1 : int list, ls2 : int list) =
  let
    fun inner_zip (ls1 : int list, ls2 : int list) =
      if null ls1
      then []
      else (hd ls1, hd ls2) :: (inner_zip (tl ls1, tl ls2))
  in
    if list_length (ls1) <> list_length (ls2)
    then NONE
    else SOME (inner_zip (ls1, ls2))
  end

val zo1 = zipOut ([1, 2, 3], [4, 5, 6, 7])
val zo2 = zipOut ([1, 2, 3], [4, 5, 6])

(* #13 *)
fun lookup (ls : (string * int) list, str : string) =
  if null ls
  then NONE
  else if (#1 (hd ls)) = str
  then SOME (#2 (hd ls))
  else lookup (tl ls, str)

val lu1 = lookup ([("hello", 1), ("baba", 2)], "asd")
val lu2 = lookup ([("hello", 1), ("baba", 2)], "baba")

(* #14 *)
fun splitup (ls : int list) =
  let
    fun gather_pos (ls : int list) =
      if null ls
      then []
      else if (hd ls) > 0
      then (hd ls) :: (gather_pos (tl ls))
      else gather_pos (tl ls)

    fun gather_neg (ls : int list) =
      if null ls
      then []
      else if (hd ls) < 0
      then (hd ls) :: (gather_neg (tl ls))
      else gather_neg (tl ls)
  in
    (gather_pos (ls), gather_neg (ls))
  end

val su1 = splitup ([1, 2, 3, 4, ~3, ~1, ~3, ~7])

(* #16 *)
fun isSorted (ls : int list) =
  if tl ls = []
  then true
  else (hd ls) < (hd (tl ls)) andalso (isSorted (tl ls))

val is1 = isSorted ([~1, 2, 3, 4, 5])

(* #17 *)
fun isAnySorted (ls : int list) =
  let
    fun desc_sorted (ls : int list) =
      if tl ls = []
      then true
      else (hd ls) > (hd (tl ls)) andalso (desc_sorted (tl ls))      
  in
    isSorted (ls) orelse (desc_sorted (ls))
  end

val ias1 = isAnySorted ([5, 4, 3, 2, 1])
val ias2 = isAnySorted ([1, 4, 3, 2, 1])

(* #18 *)
fun sortedMerge (ls1 : int list, ls2 : int list) =
  if null ls1
  then ls2
  else if null ls2
  then ls1
  else if (hd ls1) < (hd ls2)
  then (hd ls1) :: (sortedMerge ((tl ls1), ls2))
  else (hd ls2) :: (sortedMerge (ls1, (tl ls2)))

val sm1 = sortedMerge ([1, 2, 3], [~1, 5, 6, 3, 9])

(* #19 *)
fun splitAt (ls : int list, k : int) =
  let
    fun gather_pos (ls : int list) =
      if null ls
      then []
      else if (hd ls) >= k
      then (hd ls) :: (gather_pos (tl ls))
      else gather_pos (tl ls)

    fun gather_neg (ls : int list) =
      if null ls
      then []
      else if (hd ls) < k
      then (hd ls) :: (gather_neg (tl ls))
      else gather_neg (tl ls)
  in
    (gather_pos (ls), gather_neg (ls))
  end

val spla1 = splitAt([4], 4)

(* #20 *)
(*
  list: [1, 2, 3, 4]
  sortedMerge(sortedMerge([], [4], [3])
*)
(*
fun qsort (ls : int list) =
  let
    val two_lists = splitAt (ls, hd ls)
  in
    if null (#1 two_lists) orelse null (#2 two_lists)
    then sortedMerge (#1 two_lists, #2 two_lists)
    else append (sortedMerge (qsort (#1 two_lists), sortedMerge (qsort (#2 two_lists))))
  end

val qs1 = qsort ([3, 6, 1, 4, 8, 5, 6])
*)

(* #21 *)
fun divide (ls : int list) =
  let
    fun skip_divide (ls : int list, flag : bool) =
      if null ls
      then []
      else if flag
      then (hd ls) :: skip_divide ((tl ls), not flag)
      else skip_divide ((tl ls), not flag)
  in
    (skip_divide (ls, true), skip_divide (ls, false))
  end

val div1 = divide [1, 2, 3, 4, 5, 6, 7]

(* #22 *)
fun fullDivide (n : int, k : int) =
  let
    fun fd (n : int, k : int) =
      if (k mod n) <> 0
      then 0
      else 1 + fd (n, k div n)

    val d = fd (n, k)
    val n2 = k div nth_power (n, d)
  in
    (d, n2)
  end

val fd1 = fullDivide (3, 10)

(* #23 *)
(*
fun factorize (n : int) =
  let
    fun produce_pairs (n : int, divisor : int) =
      if divisor > sqrt n
      then []
      else 
        let
          val factors = fullDivide (divisor, n)
        in
          factors :: produce_pair (n, #2 factors)
        end
  in
    produce_pairs (n, 2)
  end

val fact1 = factorize (20)
*)
