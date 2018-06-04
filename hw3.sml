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
fun only_capitals xs = List.filter(fn s => Char.isUpper(String.sub(s,0))) xs

fun longest_string1 xs = List.foldl(fn (s,x) => if String.size x >= String.size s then x else s) "" xs

fun longest_string2 xs = List.foldl(fn (s,x) => if String.size x > String.size s then x else s) "" xs

fun longest_string_helper (f,acc,xs) =
    case xs of
    [] => acc
    |x::xs' => longest_string_helper (f,f(acc,x),xs')

val longest_string3 = fn xs => longest_string_helper ((fn (x,y) => if String.size x >= String.size y then x else y),"", xs)

val longest_string4 = fn xs => longest_string_helper ((fn (x,y) => if String.size x > String.size y then x else y),"",xs)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
    case xs of
    [] => raise NoAnswer
    |x::xs' => case f x of NONE => first_answer f xs' | SOME y => y

fun all_answers f xs = 
    let fun all_answers_helper g xs' = 
        case xs' of
        [] => []
        |x::es' => case g x of NONE => all_answers_helper g es' 
        | SOME y => y @ all_answers_helper g es'
    in
        let val answers = all_answers_helper f xs
        in
            if null answers then NONE
            else SOME answers
        end
    end