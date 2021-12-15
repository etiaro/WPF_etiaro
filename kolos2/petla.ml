type 'a option = None | Some of 'a
type 'a elem = {v: 'a; mutable next: 'a lista}
  and  'a lista = 'a elem option


let petla l =
  match l with
  |None -> ()
  |Some(el) ->
    let last = ref el
    in let next = ref el.next
    in while !next <> None do
      match !next with |None->()
      |Some(nextV)->
      next := nextV.next;
      nextV.next <- Some(!last);
      last := nextV;
    done;
    el.next <- Some(!last);;


let e3 = Some ({v = 3; next = None});;
let e2 = Some ({v = 2; next = e3});;
let e1 = Some ({v = 1; next = e2});;

petla e1;;

let rec sprawdzarka l = 
  match l with
  | None -> ()
  | Some (a) ->
  begin 
      print_int a.v;
      print_char ' ';
      match a.next with
      | None -> begin print_string "None\n"; (); end
      | Some (b) -> begin print_int b.v; print_newline (); end;
      sprawdzarka a.next;
  end
;;

sprawdzarka e1;;