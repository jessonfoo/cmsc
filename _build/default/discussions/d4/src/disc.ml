(* Returns elements in xs satisfying predicate f. *)
let rec filter (f : 'a -> bool) (xs : 'a list) : 'a list =
  match xs with
  |[] -> []
  |(h::t) -> if f h then h::filter f t else filter f t

(* Sets the kth element of xs to r. *)
let rec set (k : int) (r : 'a) (xs : 'a list) : 'a list =
  match xs with
  |[] -> []
  |h::t -> if k = 0 then r::set(k-1) r t else h::set(k-1) r t

let rec helper (k: int) (xs: 'a list) : 'a list =
  match xs with
  |[] -> []
  |h::t -> if k < 1 then h::helper(k-1) t else helper(k-1) t

let rec helper2 (k: int) (xs: 'a list) : 'a list = 
  match xs with
  |[] -> []
  |h::t -> if k > 0 then h::helper2(k-1) t else helper2(k-1) t

(* Rotates the list xs by k positions. *)
let rec rotate (k : int) (xs : 'a list) : 'a list =
  helper k xs @ helper2 k xs

let rec helper (k: int) (xs: 'a list) : 'a list =
  match xs with
  |[] -> []
  |h::t -> if k < 1 then h::helper(k-1) t else helper(k-1) t

let rec helper2 (k: int) (xs: 'a list) : 'a list =
  match xs with
  |[] -> []
  |h::t -> if k > 0 then h::helper2(k-1) t else helper(k-1) t

(* Return the negation of the predicate f. *)
let negate (f : 'a -> bool) : 'a -> bool =
  fun x -> if f x then false else true

(* Returns the composition of functions fs. *)
let rec composes (fs : ('a -> 'a) list) : 'a -> 'a =
  match fs with
  |[] -> fun x -> x
  |[h] -> fun a -> h a
  |h::t -> fun x ->composes [h] (composes t x)
