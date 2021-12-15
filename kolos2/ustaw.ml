open List;;

type elem = {x: int; mutable prev: elem list};;
type lista = elem list;;

let ustaw list =
  let actL = ref list
  in fold_right (fun e () -> (
    e.prev <- !actL;
    actL := tl !actL;
  )) list ();;


