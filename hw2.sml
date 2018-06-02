(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s : string , lst : string list) =
    let fun exitsOption(l) = 
        case l of
        [] => false
        |(x::xs) => if same_string(s,x) then true else exitsOption(xs)
    in
        let val flag = exitsOption(lst)
        in
            let fun a_e_o(lt) =
                case lt of 
                [] => []
                |(x'::xs') => if same_string(s,x')then a_e_o(xs') else x' ::a_e_o(xs')
            in
                if flag then SOME(a_e_o(lst)) else NONE
            end
        end
    end


fun get_substitutions1(lst : string list list , s :string) = 
    case lst of
    [] => []
    | (x::xs) => if all_except_option(s,x) = NONE 
                 then get_substitutions1(xs,s) 
                 else getOpt(all_except_option(s,x),[]) @ get_substitutions1(xs,s)

fun get_substitutions2(lst : string list list , s :string) =
    let fun get_sub(lt: string list) = 
        if all_except_option(s,lt) = NONE
        then []
        else getOpt(all_except_option(s,lt),[])
    in
        case lst of
        [] =>[]
        | (x::xs) => get_sub(x) @ get_substitutions2(xs,s)
    end

fun similar_names(lst : string list list , {first = fir,middle = mid,last = las}) =
    let 
        val r = {first = fir,middle = mid,last = las}
        fun get_sub_similar_names(ns : string list) =
            case ns of
                [] => []
                | x::xs =>  {first = x, middle = mid,last = las} :: get_sub_similar_names(xs)
    in
        [r] @ get_sub_similar_names(get_substitutions2(lst,fir))
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
fun card_color(c :card) = 
    let val (s,r) = c 
    in
        case s of
        Clubs => Black
        |Spades => Black
        |Diamonds => Red
        |Hearts   => Red
    end

fun card_value(c : card) = 
    let val (s,r) = c 
    in
        case r of 
            Ace => 11
            |Jack  => 10
            |Queen => 10
            |King  => 10
            |Num i =>  i
    end

fun remove_card(cs,c,e) =
    let fun existCard(cs,c) = 
        case cs of
        [] => false
        |(x::xs) => if c = x then true else existCard(xs,c)
    in
        let val flag = existCard(cs,c)
        in
            if flag then
                let fun all_except_first(lt,c1,count) =
                    case lt of 
                    [] => []
                    |(x'::xs') => if count = 0 andalso x' = c1 then all_except_first(xs',c1,1) 
                                else x'::all_except_first(xs',c1,count)
                in
                    all_except_first(cs,c,0)
                end
            else
                raise e
        end
    end


fun all_same_color(cs : card list) =
    case cs of
    [] => false
    | (x::[])  => true
    | head::(neck::rest) => (card_color(head) = card_color(neck) andalso all_same_color (neck::rest))


fun sum_cards(cs : card list) =
    let fun aux(lst,total) = 
        case lst of
        [] => total
        | (x::xs) => aux(xs,card_value(x)+total)
    in
        aux(cs,0)
    end


fun score (cs: card list , goal :int) =
    let val total = sum_cards(cs)
        val same_color_flag = all_same_color(cs)
    in
        if total = goal
        then 0
        else if total > goal andalso same_color_flag
        then 3 * (total - goal) div 2
        else if total > goal 
        then total - goal
        else if total < goal andalso same_color_flag
        then (goal - total) div 2
        else goal - total
    end

fun officiate(cs , moves , goal) =
    let
        fun play_moves(cs: card list, held_cards: card list, moves: move list) =
            case moves of
                [] => held_cards
              | Discard c::ms => play_moves(cs, remove_card(held_cards, c, IllegalMove), ms)
              | Draw::ms =>
                case cs of
                    [] => held_cards
                  | c::rest_of_cards =>
                    if (card_value(c) + sum_cards(held_cards)) > goal
                    then c::held_cards else play_moves(rest_of_cards, c::held_cards, ms)
    in
        score(play_moves(cs, [], moves), goal)
    end
