(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


(*1a*)
fun all_except_option(s, list_string) =
    case list_string of
      [] => NONE
    | head::tail => if same_string(s, head)
                    then SOME tail
                    else case all_except_option(s, tail) of
                            NONE => NONE
                          | SOME list_string' => SOME(head::list_string')

(*1b*)
fun get_substitutions1(list_substitutions, s) =
    case list_substitutions of
      [] => []
    | head::tail => let val aux = get_substitutions1(tail, s)
                    in  case all_except_option(s, head) of
                          NONE => aux
                        | SOME list_string => list_string @ aux
                    end

(*1c*)
fun get_substitutions2(list_substitutions, s) =
    let fun f(list_substitutions, acc) =
            case list_substitutions of
              [] => acc
            | head::tail => case all_except_option(s, head) of
                                NONE => f(tail, acc)
                              | SOME list_string => f(tail, acc @ list_string)
    in
        f(list_substitutions, [])
    end


(*1d*)
fun similar_names(list_substitutions, {first=x, middle=y, last=z}) =
    let
        fun helper_fun(aux) =
            case aux of
              [] => []
            | head::tail => {first=head, middle=y, last=z}::helper_fun(tail)
    in
        helper_fun(x::get_substitutions2(list_substitutions, x))
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove


fun card_color(suit, rank) =
    case suit of
          Clubs    => Black
        | Diamonds => Red
        | Hearts   => Red
        | Spades   => Black


fun card_value(suit, rank) =
    case rank of
      Num i => i
    | Ace   => 11
    | _     => 10


fun remove_card(cs, c, e) =
    let
    fun helper_fun cs =
        case cs of
          []         => []
        | head::tail => if head = c
                        then tail
                        else case helper_fun(tail) of
                               []  => raise e
                             | cs' => head::cs'
    in helper_fun cs end


fun all_same_color cs =
    case cs of
      []                   => true
    | head::[]             => true
    | head::(second::rest) => if (card_color(head) = card_color(second))
                              then true andalso all_same_color(second::rest)
                              else false

fun sum_cards cs =
    let fun helper(cs, acc) =
            case cs of
              []         => acc
            | head::tail => helper(tail, acc + card_value head)
    in
        helper(cs, 0)
    end


fun score(held_cards, goal) =
    let val sum = sum_cards held_cards
        fun preliminary_score sum = if sum > goal
                                    then 3 * (sum - goal)
                                    else goal - sum
    in
        case (all_same_color held_cards) of
          true  => preliminary_score(sum) div 2
        | false => preliminary_score(sum)
    end


fun officiate(card_list, move_list, goal) =
    let fun helper(card_list, held_cards, move_list) =
        case move_list of
          [] => score(held_cards, goal)
        | (Discard c)::tail_move => (case held_cards of
                                      [] => raise IllegalMove
                                    | _  => helper(card_list, remove_card(held_cards, c, IllegalMove), tail_move))
        | Draw::tail_move => case card_list of
                               []      => score(held_cards, goal)
                             | head::_ => if sum_cards(head::held_cards) > goal
                                             then score(head::held_cards, goal)
                                             else helper(remove_card(card_list, head , IllegalMove), head::held_cards, tail_move)
    in
        helper(card_list, [], move_list)
    end
