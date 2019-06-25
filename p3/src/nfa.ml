
(* NFA Type *)

type ('q, 's) transition = 'q * 's option * 'q
type ('q, 's) t = {
  qs : 'q list;
  ss : 's list;
  ts : ('q, 's) transition list;
  q0 : 'q;
  fs : 'q list;
}

(* Utility *)

(* Split a string up into a list of characters. *)
let explode (s : string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* Part 1: NFAs *)

(* Returns the move set of qs on s. *)
let rec uniq lst =
  match lst with
  |[] -> []
  |h::t -> if List.mem h lst then uniq t else h::(uniq t)

let rec move_help state value transition_list =
  match transition_list with
  |[] -> []
  |(s,m,l)::t -> if state = s && m = value then l::(move_help state value t) else (move_help state value t)

let rec move (m : ('q, 's) t) (qs : 'q list) (s : 's option) : 'q list =
  match qs with
  |[] -> []
  |h::t -> uniq((move_help h s m.ts)@(move m t s))

let rec same_states value transitions_list =
  match transitions_list with
  |[] -> [value]
  |(start,middle,last)::t -> if start = value && middle = None && not (List.mem last (same_states value t)) then last::(same_states value t) else (same_states value t)

let rec extraneous_same_states lst transitions_list =
  match lst with
  |[] -> []
  |h::t -> (same_states h transitions_list)@(extraneous_same_states t transitions_list)

(* Returns the epsilon closure of qs. *)
let rec e_closure (m : ('q, 's) t) (qs : 'q list) : 'q list =
  match qs with
  |[] -> []
  |h::t -> uniq((extraneous_same_states (same_states h m.ts) m.ts)@(e_closure m t))

let rec nfa_member state value transition_list =
  match transition_list with
  |[] -> false
  |(s,m,l)::t -> if state = s && m = value then true else nfa_member state value t

let rec intersection a b =
  match a,b with
  |[], [] -> []
  |[], _ -> []
  |_, [] -> []
  |h1::t1, h2::t2 -> if List.mem h1 a && List.mem h1 b then h1::(intersection  t1 b) else intersection t1 b

(* Returns whether the NFA m accepts string s. *)
let accept (m : ('q, char) t) (s : string) : bool =
  if (intersection m.fs (List.fold_left (fun ch a -> (e_closure m (move m a (Some ch)))) (explode s) (e_closure m [m.q0]))) > 0 then true else false

(* Part 2: Subset Construction *)

(* Converts an NFA to a DFA via the subset construction. *)
let rec dfa_of_nfa (m : ('q, 's) t) : ('q list, 's) t =
  failwith "not implemented"
