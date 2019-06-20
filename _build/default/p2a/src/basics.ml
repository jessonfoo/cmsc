(******************************)
(* Part 1: Non-List Functions *)
(******************************)

let pyth a b c = if (a > 0) && (b > 0) && (c > 0) && ((a * a) + (b * b) = (c * c)) then true else false;;

let rec gcd a b = if b = 0 then a else gcd b (a mod b);;

let reduced_form numer denom = if gcd numer denom = 0 then numer, denom else (numer / gcd numer denom), (denom / gcd numer denom);;

let rec cubes n = n * n * n + if n > 0 then cubes (n - 1) else 0;;

let rec ack m n = 
  if m = 0 then 
    n + 1
  else
    if m > 0 && n = 0 then
      ack (m - 1) 1
    else
      ack (m - 1) (ack m (n - 1))
;;

(*********************************)
(* Part 2: List Functions *)
(*********************************)

let max_first_three lst = 
  match lst with
  |[] -> -1
  |h::[] -> h
  |h1::h2::[] -> max h1 h2
  |h1::h2::h3::t -> max (max h1 h2) h3

let rec count_occ lst target = 
  match lst with
  |[] -> 0
  |h::t -> if h=target then 1 + (count_occ t target) else count_occ t target

let rec member x lst =
  match lst with
  |[] -> false
  |h::t -> if h=x then true else member x t

let rec uniq lst =
  match lst with
  |[] -> []
  |h::t -> if (member h t) = true then (uniq t) else h::(uniq t)

let rec assochelp orig newlst =
  match newlst with
  |[] -> []
  |h::t -> (h, count_occ orig h)::(assochelp orig t)

let rec assoc_list lst = 
  match uniq lst with
  |[] -> []
  |h::t -> (h, count_occ lst h)::(assochelp lst t)

let rec zip lst1 lst2 =
  match lst1, lst2 with
  |[], [] -> []
  |[], _ -> []
  |_, [] -> []
  |h1::t1, h2::t2 -> (h1, h2)::zip t1 t2

(****************)
(* Part 3: Sets *)
(****************)

let rec elem x a =
  match a with
  |[] -> false
  |h::t -> if h=x then true else member x t

let rec insert x a =
  match a with
  |[] -> [x]
  |h::t -> if elem x a then a else x::a

let rec subset a b =
  match uniq a with
  |[] -> true
  |h::t -> if (elem h b) && (count_occ a h) <= (count_occ b h) then subset t b else false

let eq a b =
  if (subset a b) && (subset b a) then true else false

let rec remove x a =
  match a with
  | [] -> []
  | h::t -> if h!=x then h::(remove x t) else (remove x t)

let rec union a b = 
  match (a,b) with
  |[], [] -> []
  |[], _ -> b
  |_, [] -> a
  |h1::t1, h2::t2 -> uniq (h1::(union t1 b))

let rec intersection a b =
  match a,b with
  |[], [] -> []
  |[], _ -> []
  |_, [] -> []
  |h1::t1, h2::t2 -> if (elem h1 a) && (elem h1 b) then h1::(intersection t1 b) else (intersection t1 b)

let rec product_help x = failwith "unimplemented"

let rec product a b =
  match a,b with
  |[], [] -> []
  |[], _ -> []
  |_, [] -> []
  |h1::t1, h2::t2 -> (h1,h2)::(union (product t1 b) (product [h1] t2))
