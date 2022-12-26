fun n_times (f, n, x) =
  if n = 0
  then x
  else f (n_times (f, n - 1, x))

fun double x = x + x
val x1 = n_times (double, 4, 7)

fun filter (f, xs) =
  case xs of
    [] => []
    | x :: xsp => if f x
                then x :: (filter (f, xsp))
                else filter (f, xsp)

(* uses lexical scope and a closure for "n" *)
fun allGreaterThan (xs, n) = filter ((fn x => x > n), xs)

fun fold (f, acc, xs) =
  case xs of
    [] => acc
    | x :: xsp => fold (f, f (acc, x), xsp)

fun numberInRange (xs, lo, hi) =
  fold ((fn (x, y) => x + (if y >= lo andalso y <= hi then 1 else 0), 0, xs))

val nir1 = numberInRange ([1, 2, 3, 4, 5], 2, 4)

fun sqrt_of_abs i = (Math.sqrt o Real.fromInt o abs) i

fun map (f, xs) =
  case xs of
    [] => []
    | x :: xsp => (f x) :: (map (f, xsp))

fun increment n = n + 1
val t4 = map (increment, [1, 2, 3, 4, 5])

val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x
val sorted3_sugar x y z = z >= y andalso y >= x
val s3 = sorted3 1 3 2
