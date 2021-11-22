(* iSet *)
(* Autor: Jakub Klimek *)
(* Reviewer: Marcin Zolek *)


type t = 
  |Empty
  |Node of t * (int * int) * t * int * int;;

let height = function
  |Node (_,_,_, h, _)-> h
  |Empty-> 0;;

let size = function
  |Node(_,_,_,_,s)->s
  |Empty -> 0;;

  (* JUST FOR DEBUGGING
let max_diff = 2;;
let check_balanced t =
  let rec aux t = 
    match t with
    |Empty -> true
    |Node(l, _, r, _, _) -> if 
      abs ((height l) - (height r)) <= max_diff && 
      aux l && 
      aux r 
    then true
    else failwith ("Tree NOT BALANCED! diffrence:"^(string_of_int (abs ((height l) - (height r)))))
  in let _ = aux t
  in t;;
  *)

let empty = Empty;;

(*operacje dodawania i odejmowania z obsługiwaniem integer overflow*)
let (++) a b =
  if a > 0 && b > 0 && a+b <= 0 then max_int
  else if a < 0 && b < 0 && a+b >= 0 then min_int
  else a+b;;
let (--) a b =
  if a >= 0 && b < 0 && a-b <= 0 then max_int
  else if a < 0 && b > 0 && a-b >= 0 then min_int
  else a-b;;

let is_empty t =
  t = Empty;;
  
  (* balansuje drzewo(wywołuje sie rekurencyjnie tak dlugo az drzewo bedzie "ładne") *)
let rec balance l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
      if height ll >= height lr then balance ll lk (balance lr k r)
      else
        (match lr with
        | Node (lrl, lrk, lrr, _, _) ->
          balance (balance ll lk lrl) lrk (balance lrr k r)
        | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then balance (balance l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
            balance (balance l k rll) rlk (balance rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, r, max hl hr + 1, (size l) ++ (size r) ++ ((snd k) -- fst k ++ 1));;

(* łączy dwa drzewa *)
let rec merge t1 t2 = 
  match t1, t2 with
  |Empty,_ -> t2
  |_,Empty -> t1
  |Node(l1,v1,r1,h1,s1),Node(l2,v2,r2,h2,s2)->
    if h1 >= h2 then
      balance l1 v1 (merge r1 t2)
    else
      balance (merge t1 l2) v2 r2;;


let rec mem x t =
  match t with
  |Empty -> false
  |Node(l,(low,up),r,h,s)-> 
    if low <= x && up >= x then true
    else if x < low then mem x l
    else (* x > up *) mem x r;;

let rec iter f t =
  match t with
  |Empty -> ()
  |Node(l,v,r,_,_) -> iter f l; f v; iter f r;;

let fold f t acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc t;;

let elements t =
  let rec help t acc =
    match t with
    |Empty -> acc
    |Node(l,v,r,_, _)-> help l (v::(help r acc))
  in help t [];;


let rec below x t =
  match t with
  |Empty -> 0
  |Node(l,(low,up),r,_,s)->
    if up <= x then size l ++ (up -- low ++ 1) ++ (below x r)
    else if low <= x then (x -- low ++ 1) ++ below x l
    else below x l;;


(* zwraca najmniejszy przedzial w drzewie oraz drzewo bez niego *)
let rec remove_min = function
  | Node (Empty, v, r, _, _) -> v,r
  | Node (l, k, r, _, _) -> let v,t = remove_min l
    in v, balance t k r
  | Empty -> invalid_arg "PSet.remove_min_elt";;
                          
(* zwraca największy przedzial w drzewie oraz drzewo bez przedzialu *)
let rec remove_max =  function
  | Node (l, v, Empty, _, _) -> v,l
  | Node (l, k, r, _, _) -> let v,t = remove_max r
    in v, balance l k t
  | Empty -> invalid_arg "PSet.remove_min_elt";;
  

let rec add (low,up) t =
  let (l, _, _),(_, _, r) = (split low t, split up t)
  in let ((x, _), lRes) =
    if l = Empty || not (mem (low - 1) l) then ((low, up), l) 
    else remove_max l(* jest przedział który kończy się na low-1, więc go usuwamy i włączamy do nowego przedziału *)
  in let ((_, y), rRes) =
    if r = Empty || not (mem (up + 1) r) then ((low, up), r)
    else remove_min r   (* analogicznie *)
  in balance lRes (x, y) rRes
and split x t = 
  match t with
  |Empty -> (Empty, false, Empty)
  |Node(l,((low,up) as v),r,_,_) ->
    if low < x && x < up then (add (low, x--1) l, true, add (x++1, up) r)
    else if low < x && x=up then (add (low, x--1) l, true, r)
    else if low = x && x < up then (l, true, add (x++1, up) r)
    else if low = x && x = up then (l, true, r)
    else if x < low then 
      let (lRes, pres, rRes) = split x l in (lRes, pres, balance rRes v r)
    else
      let (lRes, pres, rRes) = split x r in (balance l v lRes, pres, rRes);;

let rec remove (low, up) t =
  let (t1,_,_),(_,_,t2) = split low t, split up t
  in merge t1 t2;; 