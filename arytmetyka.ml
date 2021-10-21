type przedzial = {
  lower: float; 
  upper: float; 
};;

(* wartosc to posortowana rosnaco lista rozlacznych przedzialow mozliwych wartosci, 
  rodzielonych na dwa przedziały na zerze *)
type wartosc = przedzial list;;


(* funkcje pomocnicze *)

(* napraw doprowadza wartosc do wygodniejszej formy:
 - dzieli przedziały [a; b] a<0 b>0 na dwa przedziały [a; -0] i [0; b]
 - poprawia odwrócone lower i upper
 - usuwa przedziały nieokreślone(zawierające nan) 
 - scala przedziały "nachodzące na siebie"(nie licząc łączących się na 0)
 - sortuje przedziały rosnąco *)
 (* napraw jest wywoływane na wyniku każdego konstruktora/modyfikatora,
  aby otrzymać wygodną i użyteczną formę typu wartosc *)
let napraw (w:wartosc) =
  (* napraw_p zamienia upper i lower jeśli upper<lower*)
  let napraw_p {lower; upper} = 
    if lower > upper then 
      {lower = upper; upper = lower} 
    else {lower; upper}

  (* poprawny_p sprawdza czy przedział jest określony  *)
  in let poprawny_p {lower; upper} = 
    not(classify_float lower = FP_nan) && not(classify_float upper = FP_nan) && 
    not(lower = upper && (lower = infinity || lower = neg_infinity))

  (* napraw_all przechodzi przez wszystkie przedziały zawarte w lista i 
  wywołuje na nich napraw_p, usuwa nieokreślone przedziały *)
  in let rec napraw_all (lista:wartosc) res = 
    match lista with
      |[] -> (res:wartosc)
      |h::t when (poprawny_p h) -> napraw_all t ((napraw_p h)::res)
      |h::t -> napraw_all t res
      
  (* pom skleja przedziały nachodzące na siebie, 
  otrzymuje posortowaną po lower listę przedziałów w lista i aktualnie rozpatrywany przedział act *)
  in let rec scal (lista:wartosc) (act:przedzial) res =
    match lista with
      |[] -> act::res
      |h::t when (h.lower <= act.upper) -> 
        scal t {lower = act.lower; upper = h.upper} res
      |h::t -> scal t h (act::res)

  (* podziel0 przechodzi po tablicy i dzieli przedziały na zerze, 
  ustawia odpowiedni znak dla przedziałów kończących/zaczynających się na 0 oraz + dla [0,0] *)
  in let rec napraw0 (lista:wartosc) res = 
    match lista with
      |[] -> res 
      |{lower; upper}::t when lower = 0. && upper = 0. ->
        napraw0 t ({lower = 0.; upper = 0.}::res)
      |{lower; upper}::t when lower = 0. ->
        napraw0 t ({lower = 0.; upper}::res)
      |{lower; upper}::t when upper = 0. ->
        napraw0 t ({lower; upper = (-0.)}::res)
      |{lower; upper}::t when not((lower<0.) = (upper<0.)) ->
        napraw0 t ({lower; upper = (-0.)}::{lower = 0.; upper}::res)
      |{lower; upper}::t ->
        napraw0 t ({lower; upper}::res)

  (* compare to komparator do sortowania listy *)
  in let compare a b = 
    if a.lower = b.lower then 
      if a.upper < b.upper then -1 else 1
    else 
      if a.lower < b.lower then -1 else 1 
    
  (* sortujemy listę naprawioną przy użyciu napraw_all i przekazujemy ją do scal, 
  utworzoną listę przekazujemy do napraw0 *)
  in match List.sort compare (napraw_all w []) with
    |h1::t1 -> napraw0 (scal t1 h1 []) []
    |[] -> ([]:wartosc);;


(* operacja wywołana dla dwóch wartosc zwraca wartosc, czyli liste przedziałów uzyskanych
 w wyniku operacji f: l1 x l2 -> result *)
let operacja (w1:wartosc) (w2:wartosc) f = 
  (* zwraca wynik łącznej funckji ext_f dla wszystkich el. z listy, 
    pomija nan, zwraca nan dla braku poprawnych watości *)
  let rec extremum_list ext_f lista = 
    match lista with
      |[] -> nan
      |h::t when (classify_float h = FP_nan) -> extremum_list ext_f t
      |h::t -> ext_f (extremum_list ext_f t) h
  in let min_list = extremum_list min
  in let max_list = extremum_list max

  (* liczenie f dla pojedynczych przedziałów(min i max z wszystkich mozliwych f p1.upper/lower p2.upper/lower) *)
  in let policz (p1:przedzial) (p2:przedzial) = {
    lower = min_list [f p1.lower p2.lower; f p1.upper p2.lower; f p1.lower p2.upper; f p1.upper p2.upper];
    upper = max_list [f p1.lower p2.lower; f p1.upper p2.lower; f p1.lower p2.upper; f p1.upper p2.upper];
  }
  
  (* zwraca liste wynikow f [lower; upper] dla kazdego z przedziałów z listy *)
  in let rec policz_liste {lower; upper} (lista:wartosc) (res:wartosc) = 
    match lista with 
      |[] -> res
      |h::t -> policz_liste {lower; upper} t ((policz {lower; upper} h)::res) 

  (* pom to funkcja pomocnicza aby uniknąć przekazywania res bezpośrednio do funkcji operacja *)
  in let rec pom (w1:wartosc) (w2:wartosc) f res = 
    match w1 with
      |[] -> res
      |h::t -> pom t w2 f ((policz_liste h w2 [])@res)
  in napraw (pom w1 w2 f []);; 

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


(* selektory *)

let in_wartosc_p {lower; upper} x = 
  (x >= lower) && (x <= upper);;
let rec in_wartosc (w:wartosc) x = 
  match w with 
    |[] -> false
    |h::t -> in_wartosc_p h x || in_wartosc t x;;

let rec min_wartosc (w:wartosc) =
  let rec pom res left =
    match left with
      |([]:wartosc) -> res
      |h::[] -> min res h.lower
      |h::t -> pom (min res h.lower) t
  in match w with
    |([]:wartosc) -> nan
    |h::t -> pom h.lower t

let rec max_wartosc (w:wartosc) = 
  let rec pom res left =
    match left with
      |([]:wartosc) -> res
      |h::[] -> max res h.upper
      |h::t -> pom (max res h.upper) t
  in match w with
    |([]:wartosc) -> nan
    |h::t -> pom h.upper t

let sr_wartosc (w:wartosc) = (min_wartosc w +. max_wartosc w) /. 2.;;


(* operacje arytmetyczne *)

let plus a b = operacja a b (+.);;

let minus a b = operacja a b (-.);;

let razy a b = operacja a b ( *.);; 


let odwroc (w:wartosc) =
  let rec pom res lista =
    match lista with
      |[] -> res
      |h::t -> pom ({lower = 1./.h.lower; upper = 1./.h.upper}::res) t
  in napraw (pom [] w);;
let podzielic a b = razy a (odwroc b);;