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

fun similar_names(lst : string list list , r:{first:string,middle:string,last:string}) =
    let val {first=fir,middle=mid,last=las} = r
    in
        let fun get_sub_similar_name(n:string) =
            if same_string(fir,n)
            then {first:x,middle:las,last:mid}
            else {first:x,middle:las,last:mid} :: {first:x,middle:mid,last:las}
        in
            let fun get_sub_similar_names(ns: string :list) =
            case ns of
            [] => []
            | (x::xs) =>  get_sub_similar_name(n)::get_sub_similar_names(xs)
            in
                let val ns = get_substitutions2(lst,fir)
                in
                    get_sub_similar_names(ns)
                end
            end
        end 
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