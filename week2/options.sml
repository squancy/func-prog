fun better_max (xs : int list) =
  if null xs
  then NONE
  else
    let
      fun max_nonempty (xs : int list) =
        if null (tl xs)
        then hd xs
        else
          let
            val tl_ans = max_nonempty(tl xs)
          in
            if hd xs > tl_ans
            then hd xs
            else tl_ans
    in
      SOME (max_nonempty xs)
    end
