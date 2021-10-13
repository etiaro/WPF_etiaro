type wartosc = {x_1: float; x_2: float}


(* konstruktory *)

let wartosc_dokladnosc x p = {
  x_1 = x -. x *. p /. 100.;
  x_2 = x +. x *. p /. 100.
};;

let wartosc_od_do x y = {x_1 = x; x_2 = y};;

let wartosc_dokladna x = {x_1 = x; x_2 = x};;


(* funkcje *)

let in_wartosc {x_1; x_2} x = 
  (x >= x_1) && (x <= x_2);;

let min_wartosc {x_1} = x_1;;

let max_wartosc {x_2} = x_2;;

let sr_wartosc {x_1; x_2} = (x_1 +. x_2) /. 2.;;


(* funkcje pomocnicze *)
let min a b = if a < b then a else b;;
let max a b = if a > b then a else b;;

(* operacje arytmetyczne *)

let plus a b = {
  x_1 = a.x_1 +. b.x_1;
  x_2 = a.x_2 +. b.x_2;
};;

let minus a b = {
  x_1 = a.x_1 -. b.x_1;
  x_2 = a.x_2 -. b.x_2;
}

let razy a b = {
  x_1 = a.x_1 *. b.x_1;
  x_2 = a.x_2 *. b.x_2;
};;

let podzielic a b = {
  x_1 = a.x_1 /. b.x_1;
  x_2 = a.x_2 /. b.x_2;
};;