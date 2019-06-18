open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let count_occ lst target =
  fold (fun a x -> if x=target then a+1 else a) 0 lst

let uniq lst = 
  fold (fun a x -> if (count_occ a x) > 0 then a else x::a) [] lst

let assoc_list lst =
  fold (fun a x -> (x,(count_occ lst x))::a) [] (uniq lst)

let flat_map f lst =
  fold_right (fun x a -> (f x) @ a) lst []

let ap fns args = 
  fold_right (fun x a -> (map (fun arg_element -> x arg_element) args) @ a) fns []
