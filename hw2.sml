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
                 else all_except_option(s,x) :: get_substitutions1(xs,s)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)