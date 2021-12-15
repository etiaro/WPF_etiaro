let niemalejacy arr =
  if Array.length arr < 2 || arr.(0) >= 0 then true
  else
    let r = ref (Array.length arr - 1)
    in let l = ref 0
    in let s = ref 0
    in (while !l <> !r do 
      s := (!l + !r + 1) / 2;
      if arr.(!s) < 0 then 
        l := !s
      else
        r := (!s-1)
    done); arr.(!l) = arr.(0) && (!l+1 = Array.length arr || (-arr.(!l)) <= arr.(!l+1));;