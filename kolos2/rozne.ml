exception Id;;

let rozne arr =
  if Array.length arr < 2 then true
  else
    let i = ref 1
    in begin
      while !i < Array.length arr && arr.(!i-1) > arr.(!i) do 
        incr i 
      done;
      if !i = 1 || !i = Array.length arr then 
        true
      else
        let j = ref (!i-1)
        in try 
          while !i < Array.length arr && !j >= 0 do(
            if arr.(!i) = arr.(!j) then raise Id;
            if arr.(!i) > arr.(!j) then 
              decr j
            else
              incr i
          )done; true
        with |Id -> false;
    end;;