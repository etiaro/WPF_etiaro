type expr = Add of expr * expr | Var of string;;

let rec fold_expr f_add f_var = function
| Add (l, r) -> f_add (fold_expr f_add f_var l) (fold_expr f_add f_var r)
| Var z -> f_var z;;

let optymalizuj exp =
  fold_expr (fun (lt, lst) (rt, rst) ->
      if rst > lst then
        (Add(rt, lt), max (lst+1) rst)
      else
        (Add(lt, rt), max lst (rst+1))
    ) (fun v -> (Var(v), 1)) exp |> fst;;