(* Przelewanka *)
(* Author: Jakub Klimek *)
(* Code review: Adam Greloch *)

exception Found of int;;

(* stałe i operacje arytmetyczne do hashowania *)
let m = 1000000007;;
let m2 = 1000000009;;
let x = 123161345;;
let x2 = 966123512;;
let (+%) a b = (a+b) mod m;;
let (+%%) a b = (a+b) mod m2;;
let ( *%) a b = (a*b) mod m;; 
let ( *%%) a b = (a*b) mod m2;; 

let przelewanka arr =
  (* odfiltrowujemy zera - nie zmieniaja wyniku *)
  let arr = Array.of_list (Array.to_list arr |> List.filter (fun (x, _) -> x>0)) in
  try 
    (* ify na "proste" corner case - pusta tablica, 
      nieosiągalne wartości(nwd wielkości nie dzieli jakiejś docelowej),
      docelowe wartości nie zawierają żadnej pustej/pełnej *)
    if Array.length arr = 0 then raise (Found 0);
    let rec nwd a b = if max a b mod min a b = 0 then min a b else (nwd (max a b mod min a b) (min a b)) in
    let nwdall = Array.fold_left (fun r (e, _) -> if e <> 0 then nwd r e else r) (fst arr.(0)) arr in
    Array.iter (fun (_, el) -> if el mod nwdall <> 0 then raise (Found (-1))) arr;
    if Array.for_all (fun (x,y) -> y <> 0 && y <> x) arr then raise (Found (-1));

    let n = Array.length arr in
    (* funkcje hashujące - podwójne hashowanie: bardzo niska szansa na kolizję *)
    let pows = (Array.make n 1) in
    let pows2 = (Array.make n 1) in
    let p = ref 1 in
    let p2 = ref 1 in
    for i = 0 to n-1 do
      pows.(i) <- !p;
      pows2.(i) <- !p2;
      p := !p *% x;
      p2 := !p2 *%% x2;
    done;
    let get_hash s = Array.fold_left (fun (i, v, v2) e -> 
      (i+1, 
      v +% (pows.(i) *% e), 
      v2 +%% (pows2.(i) *%% e))
    ) (0, 0, 0) s |> (fun (_,a,b)->a,b) in
    let change_hash old pos dif = (fst old +% (pows.(pos) *% dif) +% m, 
      snd old +%% (pows2.(pos) *%% dif) +%% m2) in

    (*BFS po stanach*)
    let states = Hashtbl.create 4242424 in
    Hashtbl.add states (get_hash (Array.map snd arr)) false; 
    let q = Queue.create () in
    let addState s h n =
      if Hashtbl.mem states h then (
        if Hashtbl.find states h = false then raise (Found n)
      ) else (
        Hashtbl.add states h true;
        Queue.add (s, n, h) q 
      )in
    addState (Array.make n 0) (0,0) 0;
    while not (Queue.is_empty q) do
      let state, cnt, hash = Queue.pop q in
      for i=0 to n-1 do
        let newState = Array.copy state in 
        newState.(i) <- fst arr.(i); (*dolanie do pełna*)
        addState newState (change_hash hash i (newState.(i) - state.(i))) (cnt+1);
        let newState = Array.copy state in 
        newState.(i) <- 0;           (*wylanie*)
        addState newState (change_hash hash i (newState.(i) - state.(i))) (cnt+1);
        for j=0 to n-1 do 
          if j <> i then begin
            let newState = Array.copy state in  (* przelanie z i do j*)
            newState.(j) <- min (fst arr.(j)) (state.(j)+state.(i));
            newState.(i) <- state.(i) - (newState.(j) - state.(j));
            let h = change_hash hash i (newState.(i) - state.(i)) in
            addState newState (change_hash h j (newState.(j) - state.(j))) (cnt+1);
          end
        done;
      done;
    done; -1;(*nie znaleziono rozwiązania we wszystkich dostepnych stanach*)
  with |Found x -> x;;
