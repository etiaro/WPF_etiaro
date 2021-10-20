type przedzial = {
  lower: float; 
  upper: float; 
};;

(* wartosc to posortowana rosnaco lista rozlacznych przedzialow mozliwych wartosci *)
type wartosc = przedzial list;;


(* funkcje pomocnicze *)

(* napraw doprowadza wartosc do przyjaznej formy:
 - dzieli przedziały [a;b] a<0 b>0 na dwa przedziały [a;-0] i [0;b]
 - poprawia odwrócone lower i upper
 - usuwa przedziały nieokreślone(zawierające nan) 
 - scala przedziały "nachodzące na siebie"(nie licząc łączących się na 0)
 - jako efekt uboczny sortuje przedziały rosnąco *)
let napraw (l1:wartosc) =
  (* napraw_p zamienia upper i lower jeśli upper<lower*)
  let napraw_p {lower; upper} = 
    if lower > upper then {lower=upper;upper=lower} else {lower;upper}
  (* poprawny_p sprawdza czy przedział zawiera nan(czy jest określony) *)
  in let poprawny_p {lower; upper} = 
    not(classify_float lower = FP_nan) && not(classify_float upper = FP_nan) && 
    not(lower=upper && (lower = infinity || lower = neg_infinity))
  (* napraw_all ogonowo przechodzi przez wszystkie przedziały w l i 
  wywołuje na nich napraw_p <przedział>, usuwa niepoprawne *)
  in let rec napraw_all (l:wartosc) res = 
    match l with
      |[] -> (res:wartosc)
      |h::t when (poprawny_p h) -> napraw_all t ((napraw_p h)::res)
      |h::t -> napraw_all t res
  (* pom ogonowo skleja przedziały nachodzące na siebie, 
  otrzymuje posortowaną po lower listę przedziałów l i aktualnie rozpatrywany przedział act *)
  in let rec pom (l:wartosc) (act:przedzial) res =
    match l with
      |[] -> act::res
      |h::t when (h.lower <= act.upper) -> 
        pom t {lower=act.lower; upper=h.upper} res
      |h::t -> pom t h (act::res)
  (* podziel0 przechodzi po tablicy i dzieli przedziały na zerze, ustawia odpowiedni znak dla przedziałów
  kończących się na 0 i + dla <0,0> *)
  in let rec podziel0 (l:wartosc) res = 
    match l with
    |[] -> res 
    |{lower;upper}::t -> 
      if not(lower = 0.) && not(upper=0.) && not((lower<0.) = (upper<0.)) then
        podziel0 t ({lower;upper=(-0.)}::{lower=0.;upper}::res)
      else if lower=0. && upper=0. then
        podziel0 t ({lower=0.;upper=0.}::res)
      else if lower=0. then podziel0 t ({lower=0.;upper}::res)
      else if upper=0. then podziel0 t ({lower;upper=(-0.)}::res)
      else podziel0 t ({lower;upper}::res)
  (* compare to komparator do sortowania listy *)
  in let compare a b = 
    if a.lower = b.lower then 
      if a.upper < b.upper then -1 else 1
    else 
      if a.lower < b.lower then -1 else 1 
  (* sortujemy listę naprawioną przy użyciu napraw_all i przekazujemy ją do pom, utworzoną listę dzilimy na 0 *)
  in match List.sort compare (napraw_all l1 []) with
    |h1::t1 -> podziel0 (pom t1 h1 []) []
    |[]->([]:wartosc);;


(* operacja wywołana dla dwóch wartosc zwraca naprawioną wartosc, czyli liste przedziałów uzyskanych
 w wyniku operacji ∀(a z l1) ∀(b z l2) f a b *)
let rec operacja (l1:wartosc) (l2:wartosc) f res = 
  (* zwraca minimum z listy , pomija nan, zwraca nan dla braku poprawnych watości *)
  let rec min_list list = match list with
    |[] -> nan
    |h::t when (classify_float h = FP_nan) -> min_list t
    |h::t -> min (min_list t) h
  in let rec max_list list = match list with
    |[] -> nan
    |h::t when (classify_float h = FP_nan) -> max_list t
    |h::t -> max (max_list t) h
  (* liczenie f dla pojedynczych przedziałów(min i max z wszystkich mozliwych f p1.upper/lower p2.upper/lower) *)
  in let policz (p1:przedzial) (p2:przedzial) = {
        lower = min_list [f p1.lower p2.lower;f p1.upper p2.lower;f p1.lower p2.upper; f p1.upper p2.upper];
        upper = max_list [f p1.lower p2.lower;f p1.upper p2.lower;f p1.lower p2.upper; f p1.upper p2.upper];
      }
  (* liczenie f [lower;upper] dla kazdego z przedziałów w l2 *)
  in let rec policz_liste {lower;upper} (l2:wartosc) (res:wartosc) = 
          match l2 with 
            |[] -> res
            |h::t -> policz_liste {lower;upper} t ((policz {lower;upper} h)::res) 
  in match l1 with
    |[] -> napraw res
    |h::t -> operacja t l2 f ((policz_liste h l2 [])@res);;

(* konstruktory *)

let wartosc_dokladnosc x p = napraw [{
  lower = (x -. x *. p /. 100.);
  upper = (x +. x *. p /. 100.)
}];;

let wartosc_od_do x y = napraw [{
  lower = x;
  upper = y;
}];;

let wartosc_dokladna x = napraw [{
  lower = x;
  upper = x;
}];;


(* funkcje *)

let in_wartosc_p {lower; upper} x = 
  (x >= lower) && (x <= upper);;
let rec in_wartosc (l:wartosc) x = match l with 
  |[] -> false
  |h::t -> in_wartosc_p h x || in_wartosc t x;;

let rec min_wartosc (l:wartosc) =
  let rec pom res = function
    |([]:wartosc) -> res
    |h::[] -> min res h.lower
    |h::t -> pom (min res h.lower) t
  in match l with
    |([]:wartosc) -> nan
    |h::t -> pom h.lower t

let rec max_wartosc (l:wartosc) = 
  let rec pom res = function
    |([]:wartosc) -> res
    |h::[] -> max res h.upper
    |h::t -> pom (max res h.upper) t
  in match l with
    |([]:wartosc) -> nan
    |h::t -> pom h.upper t

let sr_wartosc (l:wartosc) = (min_wartosc l +. max_wartosc l) /. 2.;;


(* operacje arytmetyczne *)

let plus a b = operacja a b (+.) [];;

let minus a b = operacja a b (-.) [];;

let razy a b = operacja a b ( *.) [];; 


let odwroc (l:wartosc) =
  let rec pom res = function
    |[] -> res
    |h::t -> pom ({lower = 1./.h.lower;upper = 1./.h.upper}::res) t
  in napraw (pom [] l);;
let podzielic a b = operacja a (odwroc b) ( *.) [];;