let rzadkie n = 
  let rec licz l =
    match l with
    |_ when l > n -> 0
    |_ when l mod 2 = 1 -> 1+licz (l*2)
    |_ -> 1+licz (l*2)+licz(l*2+1)
  in licz 1;;


let rzadk n =
  let rec pewne n fib1 fib2 res =
    if n=1 then res else pewne (n/2) (fib1+fib2) fib1 (res+fib2)
  in let rec ostatn n = if n=1 then (1,1,0)
    else match ostatn (n/2) with
      |(last, c1, c0) when last=0 && n mod 2 = 0 -> (0, max (c0-1) 0, c1+c0)
      |(last, c1, c0) when last<2 && last <> n mod 2 -> (n mod 2, c0, c1+c0)
      |(last, c1, c0) -> (2, c0, c1+c0)
  in (fun (_,c1,c0) -> c1+c0) (ostatn n) + pewne n 1 1 0;;