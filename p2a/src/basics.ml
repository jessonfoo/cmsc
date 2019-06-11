(******************************)
(* Part 1: Non-List Functions *)
(******************************)

let pyth a b c = if (a * a) + (b * b) = (c * c) then true else false;;

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
      if m>0 && n>0 then
        ack (m - 1) (ack m (n - 1))
      else
        0
;;

(*********************************)
(* Part 2: List Functions *)
(*********************************)

let max_first_three lst = failwith "unimplemented"

let count_occ lst target = failwith "unimplemented"

let uniq lst = failwith "unimplemented"

let assoc_list lst = failwith "unimplemented"

let rec zip lst1 lst2 = failwith "unimplemented"

(****************)
(* Part 3: Sets *)
(****************)

let rec elem x a = failwith "unimplemented"

let rec insert x a = failwith "unimplemented"

let rec subset a b = failwith "unimplemented"

let rec eq a b = failwith "unimplemented"

let rec remove x a = failwith "unimplemented"

let rec union a b = failwith "unimplemented"

let rec intersection a b = failwith "unimplemented"

let rec product_help x b = failwith "implement if needed"

let rec product a b = failwith "unimplemented"
