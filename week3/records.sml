val x = {
  bar = ("hello", 2 + 2),
  hello = (false, true),
  bye = (false andalso true)
};

datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

fun f (x : mytype) =
  case x of
    Pizza => 3
    | Str s => 8
    | TwoInts (i1, i2) => i1 + i2

datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace | Num of int
datatype id = StudentNum of int 
            | Name of string * (string option) * string

datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp

fun eval e =
  case e of
    Constant i => i
    | Negate e2 => ~ (eval e2)
    | Add (e1, e2) => (eval e1) + (eval e2)
    | Multiply (e1, e2) => (eval e1) * (eval e2)

val e = Add (Constant 5, Constant 3)
val res = eval e

fun max_constant e = 
  case e of
    Constant i => i
    | Negate e2 => max_constant e2
    | Add (e1, e2) => Int.max (max_constant e1, max_constant e2)
    | Multiply (e1, e2) => Int.max (max_constant e1, max_constant e2)

val mexp = Add (Negate (Constant 4), Constant 9)
val max = max_constant mexp (* 9 *)

type card = suit * rank

datatype my_int_list = Empty
                     | Cons of int * my_int_list

val myls = Cons (5, Cons (4, Cons (3, Cons (2, Empty))))

fun append_mylist (xs, ys) =
  case xs of
    Empty => ys
    | Cons (x, xsp) => Cons (x, append_mylist (xsp, ys))

fun inc_or_zero intopt =
  case intopt of
    NONE => 0
    | SOME i => i + 1

fun sum_list xs =
  case xs of
    [] => 0
    | x::xsp => x + sum_list xsp

fun full_name {first = x, second = y, last = z} =
  x ^ " " ^ y ^ " " ^ z

fun len xs =
  case xs of
    [] => 0
    | _::xsp => 1 + len xsp
 
val lls1 = len [1, 2, 3, 4]

datatype sgn = P | N | Z

fun multsign (x1, x2) =
  let
    fun sign x =
      if x > 0
      then P
      else if x < 0
      then N
      else Z
  in
    case (sign x1, sign x2) of
      (Z, _) => Z
      | (_, Z) => Z
      | (P, P) => P
      | (N, N) => P
      | _ => N
  end

val tst1 = multsign (3, ~1)
