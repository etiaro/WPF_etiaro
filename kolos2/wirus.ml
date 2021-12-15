open List;;


let wirus list =
  let tmp = (fold_left (fun (acc, i) (inT, outT) -> ((0, inT, i)::(1, outT, i)::acc, i+1)) ([], 0) list)
  in let ev = sort (fun (typ1, time1, _) (typ2, time2, _) -> 
    if time1 = time2 then 
      if typ1 = typ2 then 0 
      else 
        if typ1 < typ2 then 1 else -1
    else if time1 < time2 then -1 else 1) (fst tmp)
  in let iOutNum = Array.make (snd tmp) 0
  in let res = Array.make (snd tmp) 0
  in let outNum = ref 0
  in let insideNum = ref 0
  in (iter (fun (typ, time, i) -> 
    if typ=0 then (incr insideNum; iOutNum.(i) <- !outNum;)
    else (
      decr insideNum;
      res.(i)<- !insideNum + !outNum - iOutNum.(i);
      incr outNum;
    )) ev; Array.to_list res);;