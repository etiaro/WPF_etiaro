open List;;

let przestaw arr =
  let stack = ref []
  in let i = ref 0
  in let j = ref 0
  in (
    while arr.(!i) < 0 do
      stack := (arr.(!i))::!stack;
      incr i;
    done;
    while !i < Array.length arr do
      if !stack = [] then (arr.(!j) <- arr.(!i); incr i)
      else (
        if -(hd !stack) < arr.(!i) then(
          arr.(!j) <- hd !stack;
          stack := tl !stack
        )else(
          arr.(!j) <- arr.(!i);
          incr i
        )
      );
      incr j
    done;
    while !stack <> [] do 
      arr.(!j)<- hd !stack;
      stack := tl !stack;
      incr j;
    done
  );;