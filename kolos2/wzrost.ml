open List;;

let wzrost = function
  |[] -> []
  |h::t -> fold_left (fun (l, len, bestL, bestLen) el -> 
    let (newL, newLen) = if el > (hd l) then (el::(hd l)::(tl l), len+1) else ([el],1)
    in if newLen > bestLen then (newL, newLen, newL, newLen)
    else (newL, newLen, bestL, bestLen)
  ) ([h], 1, [h], 1) t
  |> (fun (_, _, l,_)-> rev l);;