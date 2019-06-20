type 'a listy =
  Nil
| Cons of 'a * 'a listy

(* Applies f to all elements of xs. *)
let rec map (f : 'a -> 'b) (xs : 'a listy) : 'b listy =
  match xs with
  |Nil -> Nil
  |Cons(h,t) -> Cons((f h),(map f t))

(* Folds left-to-right over xs with f and accumulator a. *)
let rec foldl (f : 'a -> 'b -> 'a) (a : 'a) (xs : 'b listy) : 'a =
  match xs with
  |Nil -> a
  |Cons(h,t) -> foldl f (f a h) t

(* Folds right-to-left over xs with f and accumulator a. *)
let rec foldr (f : 'b -> 'a -> 'a) (xs : 'b listy) (a : 'a) : 'a =
  match xs with
  |Nil -> a
  |Cons(h,t) -> f h (foldr f t a)

(* Returns (as an option) the last element that satisfies predicate p.
   Don't make find recursive. *)
let find (p : 'a -> bool) (xs : 'a listy) : 'a option =
  let satisfied_p_lst = foldl (fun a x -> if p x then x::a else a) [] xs in
  match satisfied_p_lst with
  |[] -> None
  |h::t -> Some h

(* Returns if xs contains element e. Don't make contains recursive. *)
let contains (xs : 'a listy) (e : 'a) : bool =
  let contains_lst = foldl (fun a x -> if x=e then x::a else a) [] xs in
  match contains_lst with
  |[] -> false
  |h::t -> true

