let cykl arr =
  let vis = Array.make (Array.length arr) false
  in let res = ref 0
  in let rec dfs i num = 
    if vis.(i) then (
      res := (max (!res) num);
    ) else (
      vis.(i) <- true;
      dfs arr.(i) (num + 1);
    )
  in let rec loop i =
    if i < Array.length arr then (
      dfs i 0;
      loop (i+1);
    )
  in (loop 0; !res);; 