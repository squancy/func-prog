(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* #1 *)
fun only_capitals (str_list) =
  List.filter (fn s => Char.isUpper (String.sub (s, 0))) str_list

(* #2 *)
fun longest_string1 (str_list) =
  List.foldl (fn (s, x) => if String.size (s) > String.size (x) then s else x) "" str_list 

(* #3 *)
fun longest_string2 (str_list) =
  List.foldl (fn (s, x) => if String.size (s) >= String.size (x) then s else x) "" str_list

(* #4 *)
fun longest_string_helper f str_list =
  List.foldl f "" str_list

val ls3_fun = fn (s, x) => if String.size (s) > String.size (x) then s else x
val longest_string3 = longest_string_helper ls3_fun

val ls4_fun = fn (s, x) => if String.size (s) >= String.size (x) then s else x
val longest_string4 = longest_string_helper ls4_fun

(* #5 *)
fun longest_capitalized (str_list) =
  (longest_string1 o only_capitals) str_list

(* #6 *)
fun rev_string (str) =
  (String.implode o List.rev o String.explode) str

(* #7 *)
fun first_answer f ls =
  case ls of
    [] => raise NoAnswer 
    | ls :: lsp => case f ls of
                     SOME v => v 
                     | NONE => first_answer f lsp

(* #8 *)
fun all_answers f ls =
  let
    fun loop (ls, acc) =
      case ls of
        [] => SOME acc
        | ls :: lsp => case f ls of
                         NONE => NONE
                         | SOME v => loop (lsp, acc @ v)
  in
    loop (ls, [])
  end

(* #9 *)
fun count_wildcards (p) =
  
