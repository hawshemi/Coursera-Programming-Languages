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

(* 1 *)
fun only_capitals str_lst =
  List.filter (fn str => Char.isUpper(String.sub(str, 0))) str_lst


(* 2 *)
fun longest_string1 str_lst =
  foldl (fn (x,y) => if String.size x > String.size y then x else y) "" str_lst


(* 3 *)
fun longest_string2 str_lst =
  foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" str_lst


(* 4 *)
fun longset_string_helper compare_func str_lst =
  foldl (fn (x,y) => if compare_func(String.size x,String.size y) then x else y)
        ""
        str_lst

val longest_string3 = longset_string_helper (fn (x,y) => x > y)

val longest_string4 = longset_string_helper (fn (x,y) => x >= y)


(* 5 *)
val longest_capitalized = longest_string1 o only_capitals


(* 6 *)
val rev_string = String.implode o rev o String.explode


(* 7 *)
fun first_answer f lst =
  case lst of
      [] => raise NoAnswer
    | x::xs' => case f x of
                    SOME v => v
                  | NONE => first_answer f xs'


(* 8 *)
fun all_answers f lst =
  let
      fun aux lst acc =
        case lst of
            [] => SOME acc
          | x::xs' => case f x of
                          NONE => NONE
                        | SOME y => aux xs' (acc @ y)
  in
      aux lst []
  end


(* 9-a *)
fun count_wildcards p =
  g (fn () => 1) (fn str => 0) p


(* 9-b *)
fun count_wild_and_variable_lengths p =
  g (fn () => 1) String.size p


(* 9-c *)
fun count_some_var (s, p) =
  g (fn () => 0) (fn str => if s = str then 1 else 0) p


(* 10 *)
fun check_pat p =
  let
      fun ptn_to_strlst ptn =
        case ptn of
            Variable s => [s]
          | TupleP ps => List.foldl (fn (p,i) => (ptn_to_strlst p) @ i) [] ps
          | ConstructorP(_,p) => ptn_to_strlst p
          | _ => []
      fun has_repeats str_lst =
        case str_lst of
            [] => false
          | x::xs' => if List.exists (fn y => y = x) xs' then true else
                      has_repeats xs'
      val f = not o has_repeats o ptn_to_strlst
  in
      f p
  end


(* 11 *)
fun match (v, p) =
  case (v, p) of
      (_, Wildcard) => SOME []
    | (sv, Variable sp) => SOME [(sp,sv)]
    | (Unit, UnitP) => SOME []
    | (Const iv, ConstP ip) => if iv = ip then SOME [] else NONE
    | (Tuple tv, TupleP tp) => if List.length tv = List.length tp
                               then all_answers match (ListPair.zip(tv, tp))
                               else NONE
    | (Constructor (s1,cv), ConstructorP (s2,cp)) => if s1 = s2
                                                     then match (cv,cp)
                                                     else NONE
    | (_, _) => NONE


(* 12 *)
fun first_match v p_lst =
  SOME (first_answer (fn x => match(v, x)) p_lst) handle NoAnswer => NONE
