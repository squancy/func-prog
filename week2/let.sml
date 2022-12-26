fun countup_from1 (x : int) =
  let
    fun count (from : int, to : int) =
      if from = to
      then to :: []
      else from :: count(from + 1, to)
  in
    count (1, x)
  end

fun countup_from1_better (x : int) =
  let
    fun count (from : int) =
      if from = x
      then x :: []
      else from :: count(from + 1)
  in
    count 1
  end

fun good_max (xs : int list) =
  if null xs
  then 0
  else if null (tl xs)
  then hd xs
  else
    let
      val tl_ans = good_max(tl xs)
    in
      if hd xs > tl_ans
      then hd xs
      else tl_ans
    end

