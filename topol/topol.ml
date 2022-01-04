exception Cykliczne;;


(** Sortuje topologicznie liste postaci [(v, [lista_sasiadow])...] *)
let topol list =
  (* mapa postaci map[wartosc]=lista następników *)
  let map = ref PMap.empty in
  let getNast v =
    try PMap.find v !map with
    |Not_found -> []
  (* st[i] = ilość nieodwiedzonych poprzedników i. -1 kiedy i juz byl odwiedzony *)
  in let st = ref PMap.empty in
  let getSt v = 
    try PMap.find v !st with
    |Not_found -> 0
  in List.iter (fun (x, l) -> (* wypełniamy mapy list następników i stopni *)
    map := PMap.add x (l@(getNast x)) !map;
    List.iter (fun el ->
      st := PMap.add el (getSt el + 1) !st;
    ) l;
  ) list;
  let res = ref [] in (* tutaj bedziemy dodawac odwiedzane po kolei wierzcholki *)
  let rec dfs v = (* przechodzimy po wszystkich następnikach o st=0(czyli takich, których wszystkich poprzedników już odwiedziliśmy) *)
    res := v::!res;
    st := PMap.add v (-1) !st;
    let rec loop = function
    |[] -> ()
    |u::t ->  
      begin
        let newSt = getSt u - 1 in
        st := PMap.add u newSt !st;
        if newSt = 0 then
          dfs u;
        loop t
      end 
    in loop (getNast v)
  in
  List.iter (fun (x, _) ->
    if getSt x = 0 then
      dfs x
  ) list;
  (* jeśli nie odwiedziliśmy jakiegokolwiek wierzchołka to musiał gdzieś być cykl *)
  PMap.iter (fun _ v -> if v <> -1 then raise Cykliczne) !st;
  List.rev !res;;