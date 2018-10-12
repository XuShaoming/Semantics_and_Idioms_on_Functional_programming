(* Dan Grossman, CSE341 Spring 2013, HW3 Provided Code *)

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
(*The List.filter is curry style*)
fun only_capitals xs =
    List.filter (fn x => Char.isUpper(String.sub(x,0))) xs
				       
fun longest_string1 xs =
    List.foldl (fn (x,acc) => if String.size(x) > String.size(acc)
			    then x
			    else acc
	       ) "" xs
					      
fun longest_string2 xs =
    List.foldl (fn (x,acc) => if String.size(x) >= String.size(acc)
			    then x
			    else acc
	       ) "" xs						 
(*Currying and Partial Application idiom*)	 
fun longest_string_helper f xs  =
    List.foldl (fn (x,acc) => if f(String.size(x),String.size(acc)) then x else acc) "" xs

val longest_string3 = longest_string_helper (fn (x,y) => x > y)
		
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

(*Combining funcion idiom*)
val longest_capitalized = longest_string3 o only_capitals 
						 
val rev_string = String.implode o List.rev o String.explode 

						 
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      SOME x => x
		    | NONE => first_answer f xs' 
(*tail recursive, pattern match, first class funcion, currying*) 
fun all_answers f xs =
    let fun aux acc xs =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f(x) of
			      NONE => NONE
			    | SOME v => aux (v@acc) xs'
    in aux [] xs
    end

	
(**
g takes 
val g = fn : (unit -> int) -> (string -> int) -> pattern -> int
g is used to count the number of specific characters of the pattern
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
*)
(*Currying, pattern match, recursion, anonymous function*)
fun count_wildcards p = g (fn () => 1) (fn x => 0) p	
				      
fun count_wild_and_variable_lengths p = g (fn () => 1) (fn x => String.size x) p
(*Closureon s*)					  
fun count_some_var (s,p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p 

(*List.exists use closure and curring, I use funcion combining here*)
fun check_pat p =
    let fun get_strings p =
	    case p of
		Wildcard          => []
	      | Variable x        => [x]
	      | TupleP ps         => List.foldl (fn (p,acc) => (get_strings p) @ acc) [] ps
	      | ConstructorP(_,p) => get_strings p
	      | _                 => []
	fun dist_list xs =
	    case xs of
		[] => true
	      | x::xs' => not (List.exists (fn y => y=x ) xs')					 
    in (dist_list o get_strings) p
    end

fun match (v, p) =
    case (p,v) of
	(Wildcard, _) => SOME []
      | (Variable s, _) => SOME [(s,v)]
      | (UnitP,Unit) => SOME []
      | (ConstP cp, Const ct) => if cp = ct then SOME [] else NONE
      | (TupleP ps, Tuple vs) => if List.length(ps) = List.length(vs)
				 then all_answers (fn (x,y) => match(x,y)) (ListPair.zip(vs,ps))
				 else NONE
      | (ConstructorP(s1,p), Constructor(s2,v)) => if s1=s2 then match(v,p) else NONE
      | _ => NONE 
	    
fun first_match v ps =
    SOME(first_answer (fn p => match(v,p)) ps)
    handle NoAnswer => NONE		
