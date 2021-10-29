type 'a node =  {value:'a; left:'a queue; right:'a queue;len: int} and
     'a queue = Leaf | Node of 'a node;;
 
       (* Funkcje pomocnicze *)
 let getLen = function
   |Leaf -> 0
   |Node n -> n.len;;
 
   (* funkcje ze specyfikacji *)
 let empty = Leaf;
 exception Empty;;
 let is_empty q1 = q1=empty;;
 
 let rec join q1 q2 = 
  let sortPairByVal (n1,n2) = 
     if n1.value < n2.value then (n1,n2) else (n2,n1)
  in let sortPairByLen (q1, q2) =
    if getLen q1 < getLen q2 then (q1, q2) else (q2, q1)
   
  in match q1 with
  |Leaf -> q2
  |Node n1 ->
    match q2 with
    |Leaf -> q1
    |Node n2 -> 
      let (d1, d2) = sortPairByVal (n1, n2)
  in let d3 = join d1.right (Node d2)
  in let (short, long) = sortPairByLen (d3, d1.left)
  in Node {value = d1.value; left = long; right = short; len = (getLen short)+1};; 
 
 let add v q = 
   join q (Node {value = v; left = empty; right = empty; len = 1});;
 
 let delete_min q = 
   match q with
   |Leaf -> raise Empty
   |Node q -> (q.value, join q.left q.right);;
