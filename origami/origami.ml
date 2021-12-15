(* Origami *)
(* Jakub Klimek *)
(* review: Konrad Obernikowicz *)

(** Punkt na płaszczyźnie *)
type point = float * float

(** Poskładana kartka: typ będący funkcją zwracającą ile razy kartkę przebije szpilka wbita w danym
punkcie *)
type kartka = point -> int

(** [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty
prostokąt o bokach równoległych do osi układu współrzędnych i lewym
dolnym rogu [p1] a prawym górnym [p2]. Punkt [p1] musi więc być
nieostro na lewo i w dół od punktu [p2]. Gdy w kartkę tę wbije się 
szpilkę wewnątrz (lub na krawędziach) prostokąta, kartka zostanie
przebita 1 raz, w pozostałych przypadkach 0 razy *)
let prostokat (p1x, p1y) (p2x, p2y) = function
  |(x, y) -> 
    if p1x <= x && p2x >= x && p1y <= y && p2y >= y then 1 else 0;;

(** [kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku
w punkcie [p] i promieniu [r] *)
let kolko (kx, ky) r = function
  |(x, y) -> 
      if sqrt((abs_float (kx -. x)) ** 2. +. (abs_float (ky -. y)) ** 2.) <= r then 1 else 0;;

(* [pozycja p1 p2 p] zwraca -1 jezeli p jest po prawej od prostej p1-p2
   0 jezeli na prostej(traktując bardzo małe różnice jako błąd przybliżenia typu danych float) 
   i 1 jezeli jest po lewej *)
let pozycja (p1x, p1y) (p2x, p2y) (x, y) = 
  let det = p1x *. p2y +. p2x *. y +. x *. p1y -. p1x *. y -. p2x *. p1y -. x *. p2y
  in match det with
  |_ when abs_float(det) < 1e-12 -> 0.
  |_ when det < 0. -> -1.
  |_ -> 1.;;

(* [odbij p1 p2 p] zwraca symetrie punktu p wzgledem prostej p1-p2 *)
let odbij (p1x, p1y) (p2x, p2y) (x, y) = 
  if p1x = p2x then
    (2. *. p1x -. x, y)
  else
    let a = (p2y -. p1y) /. (p2x -. p1x)
    in let b = -1.
    in let c = p1y -. a *. p1x
    in let temp = -2. *. (a *. x +. b *. y +. c) /. (a *. a +. b *. b)
    in (temp *. a +. x, temp *. b +. y);;

(** [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej
przez punkty [p1] i [p2] (muszą to być różne punkty). Papier jest
składany w ten sposób, że z prawej strony prostej (patrząc w kierunku
od [p1] do [p2]) jest przekładany na lewą. Wynikiem funkcji jest
złożona kartka. Jej przebicie po prawej stronie prostej powinno więc
zwrócić 0. Przebicie dokładnie na prostej powinno zwrócić tyle samo,
co przebicie kartki przed złożeniem. Po stronie lewej - tyle co przed
złożeniem plus przebicie rozłożonej kartki w punkcie, który nałożył
się na punkt przebicia. *)
let zloz ((p1x, p1y) as p1) ((p2x, p2y) as p2) k ((x, y) as p) =
  match pozycja p1 p2 p with
  |0. -> k p
  |(-1.) -> 0
  |v -> k p + k (odbij p1 p2 p);;

(** [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)] 
czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych 
z listy *)
let skladaj l k = List.fold_left (fun res (p1, p2) -> zloz p1 p2 res) k l;;
