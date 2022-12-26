type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

(* #1 *)
fun pass_or_fail ({id = _, grade = g}) =
  case g of
    SOME (i) => if i >= 75
                then pass
                else fail
    | NONE => fail

(* #2 *)
fun has_passed (fgrade) =
  let
    val pof = pass_or_fail (fgrade)
  in
    case pof of
      pass => true
      | fail => false
  end

(* #3 *)
fun number_passed (log) =
  case log of
    [] => 0
    | l :: ls' => if has_passed l
                  then 1 + number_passed ls'
                  else number_passed ls'

(* #4 *)
fun number_misgraded (lot) =
  case lot of
    [] => 0
    | (pass, fg) :: ls' => if has_passed fg
                           then number_misgraded ls'
                           else 1 + number_misgraded ls'
    | (fail, fg) :: ls' => if has_passed fg
                           then 1 + number_misgraded ls'
                           else number_misgraded ls'

datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

(* #5 *)
fun tree_height (tree) =
  let
    fun calc_height (tree, acc) = 
      case tree of
        leaf => acc
        | node {value = _, left = l, right = r} => 
          if calc_height (l, acc + 1) > calc_height (r, acc + 1)
          then calc_height (l, acc + 1)
          else calc_height (r, acc + 1)
  in
    calc_height (tree, 0)
  end

(* #6 *)
fun sum_tree (tree) =
  case tree of
    leaf => 0
    | node {value = v, left = l, right = r} => v + sum_tree l + sum_tree r

(* #7 *)
fun gardener (tree) =
  case tree of
    leaf => leaf
    | node {value = leave_me_alone, left = l, right = r} =>
      node ({value = leave_me_alone, left = gardener l, right = gardener r})
    | node {value = prune_me, left = l, right = r} =>
      leaf

datatype nat = ZERO | SUCC of nat

(* #8 *)
fun is_positive (n) =
  case n of
    ZERO => false
    | _ => true

(* #9 *)
exception Negative

fun pred (n) =
  case n of
    ZERO => raise Negative
    | SUCC (i) => i

(* #10 *)
fun nat_to_int (n) =
  case n of
    ZERO => 0
    | SUCC (i) => 1 + nat_to_int i

(* #11 *)
fun int_to_nat (n) =
  if n < 0
  then raise Negative
  else 
    case n of
      0 => ZERO
      | _ => SUCC (int_to_nat (n - 1))

(* #12 *)
fun add (n1, n2) =
  case n1 of
    ZERO => n2
    | SUCC (i) => SUCC (add (i, n2))

(* #13 *)
fun sub (n1, n2) =
  case n2 of
    ZERO => n1
    | _ => sub (pred n1, pred n2)

(* #14 *)
fun mult (n1, n2) =
  case n1 of
    ZERO => ZERO
    | _ => add (n2, mult (pred n1, n2))

(* #15 *)
fun less_than (n1, n2) =
  case (n1, n2) of
    (ZERO, ZERO) => false
    | (ZERO, SUCC (i)) => true
    | (SUCC (i), ZERO) => false
    | (SUCC (i), SUCC (j)) => less_than (pred n1, pred n2)

val x = less_than (SUCC ZERO, SUCC ZERO)

datatype intSet =
  Elems of int list (*list of integers, possibly with duplicates to be ignored*)
| Range of { from : int, to : int }  (* integers from one number to another *)
| Union of intSet * intSet (* union of the two sets *)
| Intersection of intSet * intSet (* intersection of the two sets *)

(* #16 *)
fun isEmpty (set) =
  case set of
    Elems [] => true
    | _ => false

(* #17 *)
fun contains (set, n) =
  case set of
    Elems [] => false
    | Elems (ls :: ls') => if ls = n
                           then true
                           else contains (Elems ls', n)

fun toList (set) =
  let
    fun build_list (set, acc) =
      case set of
        Elems [] => acc
        | Elems (ls :: ls') => if contains (Elems acc, ls)
                               then build_list (Elems ls', acc)
                               else build_list (Elems ls', ls :: acc)
  in
    build_list (set, [])
  end

