open Funs

(***********************)
(* Part 2: Integer BST *)
(***********************)

type int_tree =
  | IntLeaf
  | IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(x, IntLeaf, IntLeaf)
  | IntNode (y, l, r) when x > y -> IntNode (y, l, int_insert x r)
  | IntNode (y, l, r) when x = y -> t
  | IntNode (y, l, r) -> IntNode (y, int_insert x l, r)

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (y, l, r) when x > y -> int_mem x r
  | IntNode (y, l, r) when x = y -> true
  | IntNode (y, l, r) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t =
  match t with
  |IntLeaf -> 0
  |IntNode (y, l, r) -> int_size l + int_size r + 1

let rec int_max t =
  match t with
  |IntLeaf -> invalid_arg "int_max"
  |IntNode (y, l, IntLeaf) -> y
  |IntNode (y, l, r) -> int_max r

let rec int_common t x y = 
  if (int_mem x t && int_mem y t) then
    if (x = y) then
      x
    else
  match t with
  |IntLeaf -> invalid_arg "int_common"
  |IntNode (z, l, r) when (int_mem x l) && not (int_mem y l) -> z
  |IntNode (z, l, r) when int_mem x r && not (int_mem y r) -> z
  |IntNode (z, l, r) when not (int_mem x l) && int_mem y l -> z
  |IntNode (z, l, r) when not (int_mem x r) && int_mem y r -> z
  |IntNode (z, l, r) when int_mem x l && int_mem y l -> int_common l x y
  |IntNode (z, l, r) -> int_common r x y
  else
    invalid_arg "int_common"


(***************************)
(* Part 3: Polymorphic BST *)
(***************************)

type 'a atree =
    Leaf
  | Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)

let rec pinsert x t =
  match t with
  |(f, Leaf) -> (f, Node (x, Leaf, Leaf))
  |(f, Node (y, l, r)) when (f x y) > 0  -> (f, Node(y, l, snd(pinsert x (f, r))))
  |(f, Node (y, l, r)) when (f x y) = 0 -> t
  |(f, Node (y, l, r)) -> (f, Node(y, snd(pinsert x (f, l)), r))

let rec pmem x t = 
  match t with
  |(f, Leaf) -> false
  |(f, Node (y, l, r)) when (f x y) > 0 -> pmem x (f, r)
  |(f, Node (y, l, r)) when (f x y) = 0 -> true
  |(f, Node (y, l, r)) -> pmem x (f, l)

let pinsert_all lst t =
  fold (fun a x -> pinsert x a) t lst

let rec p_as_list t = 
  match t with
  |(f, Leaf) -> []
  |(f, Node (y, l, r)) -> (p_as_list (f, l))@(y::(p_as_list (f, r)))

let rec pmap f t =
  let lst_t = p_as_list t in
  let mapped_lst = map f lst_t in
  pinsert_all mapped_lst (empty_ptree Pervasives.compare)

(*******************************)
(* Part 4: Shapes with Records *)
(*******************************)

type pt = { x: int; y: int }
type shape =
  Circ of { radius: float; center: pt }
  | Rect of { width: float; height: float; upper: pt }

let area s =
  match s with
  |Circ shape -> 3.14 *. shape.radius *. shape.radius
  |Rect shape -> shape.width *. shape.height

let filter f lst = 
  fold_right (fun x a -> if f x then x::a else a) lst []

let partition thresh lst =
  let less_than_thresh = fold_right (fun x a -> if area x < thresh then x::a else a) lst [] in
  let greater_equal_thresh = fold_right (fun x a -> if area x >= thresh then x::a else a) lst [] in
  (less_than_thresh, greater_equal_thresh)

let rec qs lst = 
  match lst with
  |[] -> []
  |h::t -> let (ys,zs) = partition (area h) t in (qs ys) @ (h::(qs zs))
