fun pow(x : int, y: int) =
  if y = 0 then 1
  else x * pow(x, y - 1) 

val x = pow(5, 7)

fun cube(x : int) = pow(x, 3)

val y = cube(3)
