(* Returns the sum of squares of every integer in xs. *)
let sq_sum (xs : int list) : int =
  let lst = List.map (fun x -> x * x) xs in List.fold_left (+) 0 lst

(* Appends ys to the end of xs (without using @ or List.append). *)
let append (xs : 'a list) (ys : 'a list) : 'a list =
  List.fold_right (fun x xt -> x::xt) xs ys

(* Returns the run length encoding of xs
   e.g. run_length [1;1;2;2;1;3;3] -> [(1,2);(2,2);(1,1);(3,2)]. *)
let run_length (xs : 'a list) : ('a * int) list =
  List.fold_right (fun t a -> if a = [] then (t, 1)::a else 
    match a with 
    |(h1, h2)::tail -> if t=h1 then (h1, h2 + 1)::tail else (t,1)::a
    ) xs []

(* Returns all elements that satisfy the predicate p (without using List.filter). *)
let filter (p : 'a -> bool) (xs : 'a list) : 'a list =
  List.fold_right (fun x a -> if p x then x::a else a) xs []

(* Returns the composition of all functions in fs. *)
let composes (fs : ('a -> 'a) list) : ('a -> 'a) =
  List.fold_right (fun t a-> t a) fs
