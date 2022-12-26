(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* (a) *)
fun all_except_option (s, ls) =
  let
    fun is_in_list ls =
      case ls of
        [] => false
        | x :: lsp => if same_string (s, x)
                      then true
                      else is_in_list lsp

    fun except ls =
      case ls of
        [] => []
        | x :: lsp => if same_string (s, x)
                      then except lsp
                      else x :: except lsp
  in
    if not (is_in_list ls)
    then NONE
    else
      case except ls of
        _ => SOME (except ls)
  end

(* (b) *)
fun get_substitutions1 (substitutions, s) =
  case substitutions of
    [] => []
    | sub :: rest => case all_except_option (s, sub) of
                       NONE => get_substitutions1 (rest, s)
                       | SOME v => v @ get_substitutions1 (rest, s)

(* (c) *)
fun get_substitutions2 (substitutions, s) =
  let
    fun substitute (substitutions, acc) =
      case substitutions of
        [] => acc
        | sub :: rest => case all_except_option (s, sub) of
                           NONE => substitute (rest, acc)
                           | SOME v => substitute (rest, v @ acc)
  in
    substitute (substitutions, [])
  end

(* (d) *)
fun similar_names (substitutions, full_name) =
  let
    val {first = first, last = last, middle = middle} = full_name
    val sub_names = get_substitutions2 (substitutions, first)
    fun generate_names ls =
      case ls of
        [] => []
        | x :: lsp => {first = x, middle = middle, last = last} :: generate_names lsp
  in
    full_name :: generate_names sub_names
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* (a) *)
fun card_color (s, r) =
  case s of
    Clubs => Black
    | Spades => Black
    | _ => Red

(* (b) *)
fun card_value (s, r) =
  case r of
    Num i => i
    | Ace => 11
    | _ => 10

(* (c) *)
fun remove_card (cs, c, e) =
  let
    fun remove (cs, is_removed) =
      case cs of
        [] => if is_removed
              then []
              else raise e
        | x :: csp => if x = c andalso not is_removed
                      then remove (csp, true)
                      else x :: remove (csp, is_removed)
  in
    remove (cs, false)
  end

(* (d) *)
fun all_same_color cs =
  case cs of
    [] => true
    | (s, r) :: [] => true
    | (s1, r1) :: (s2, r2) :: csp => if card_color (s1, r1) = card_color (s2, r2)
                                     then all_same_color ((s2, r2) :: csp)
                                     else false

(* (e) *)
fun sum_cards cs =
  let
    fun summa (cs, acc) =
      case cs of
        [] => acc
        | (s, r) :: csp => summa (csp, acc + card_value (s, r))
  in
    summa (cs, 0)
  end

(* (f) *)
fun score (cs, goal) =
  let
    val sum_of_held_cards = sum_cards cs

    fun calc_pre_score summa =
      if summa > goal
      then 3 * (summa - goal)
      else goal - summa

    val pre_score = calc_pre_score sum_of_held_cards
  in
    if all_same_color cs
    then pre_score div 2
    else pre_score
  end

(* (g) *)
fun officiate (card_list, moves, goal) =
  let
    fun run_game (card_list, moves, held_cards, sc) =
      case moves of
        [] => sc
        | Discard c :: other_moves =>
                     let
                       val new_held_cards = remove_card (held_cards, c, IllegalMove)
                     in
                       run_game (card_list, other_moves, new_held_cards,
                         score (new_held_cards, goal))
                     end
        | Draw :: other_moves =>
                  case card_list of
                    [] => sc
                    | cl :: cl' =>
                      if sum_cards (cl :: held_cards) > goal
                      then score (cl :: held_cards, goal)
                      else run_game (cl', other_moves, cl :: held_cards,
                            score (cl :: held_cards, goal))
  in
    run_game (card_list, moves, [], score ([], goal))
  end
