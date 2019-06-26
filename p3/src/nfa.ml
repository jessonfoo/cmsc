
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

(*( Part 1: NFAs *)

(* Returns the move set of qs on s. *)
let rec uniq (lst: 'q list) =
  match lst with
  |[] -> []
  |h::t -> if List.mem h t then uniq t else h::(uniq t)
  (* 
   *  
   *  *)
let rec move_help (state : int)  (value : 's option)  (transition_list)  : 'q list =
  match transition_list with
  |[] -> []
  |(s,m,l)::t -> if state = s && (m = value) then l::(move_help state value t) else (move_help state value t)

(* 
  @param qs list : state
  @param s option
  @param m
 
 *)
let rec move (m : ('q, 's) t) (qs : 'q list) (s : 's option) : 'q list =
  match qs with
  |[] -> []
  (*take each value in qs, the list of states given, and find each state it can lead to given the option s*)
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
  if List.length (intersection m.fs (List.fold_left (fun a ch -> (e_closure m (move m a (Some ch)))) (e_closure m [m.q0]) (explode s))) > 0 then true else false

(* Part 2: Subset Construction *)
let rec add_to_state_list (nq0: 'q list) (nss: 's list) (nqs: 'q list list) (nts: ('q list, 's) transition list)  (nfs: 'q list list) (m: ('q, 's) t) =
  match nqs with
  |[] -> {
    q0 = nq0;
    ss = nss;
    qs = nqs;
    ts = nts;
    fs = nfs;
  }
  |h::t -> {
    q0 = nq0;
    ss = nss;
    qs = (add_to_state_list nq0 nss (List.fold_left (fun a value -> (e_closure m (move m h (Some value)))::nqs) nqs nss) nts nfs m);
    ts = nts;
    fs = nfs;
  }


let rec add_to_transitions_list (nq0: 'q list) (nss: 's list) (nqs: 'q list list) (nts: ('q list, 's) transition list) (nfs: 'q list list) (m: ('q, 's) t) (tail_nqs: 'q list list) =
  match tail_nqs with
  |[] -> {
    q0 = nq0;
    ss = nss;
    qs = nqs;
    ts = nts;
    fs = nfs;
  }
  |h::t -> {
    q0 = nq0;
    ss = nss;
    qs = nqs;
    ts = (add_to_transitions_list nq0 nss nqs (List.fold_left (fun a value -> (h, value, e_closure m (move m h value))::nts) nts nss) nfs m t);
    fs = nfs;
  }

let add_to_final_states (nq0: 'q list) (nss: 's list) (nqs: 'q list list) (nts: ('q list, s) transition list) (nfs: 'q list) (mfs: 'q list) (nqs_tail: 'q list list) =
  match nqs_tail with
  |[] -> {
    q0 = nq0;
    ss = nss;
    qs = nqs;
    ts = nts;
    fs = nfs;
  }
  |h::t -> {
    q0 = nq0;
    ss = nss;
    qs = nqs;
    ts = nts;
    fs = uniq(add_to_final_states (nq0 nss nqs nts (List.fold_left (fun a value -> if (List.mem value h) then h::nfs else nfs) nfs mfs) t));
  }

(* Converts an NFA to a DFA via the subset construction. *)
let rec dfa_of_nfa (m : ('q, 's) t) : ('q list, 's) t =
  let n = (add_to_state_list (e_closure (m, [m.q0])) m.ss [e_closure (m, [m.q0])] [] [] m) in
  let nn = add_to_transitions_list (n.q0 n.ss n.qs n.ts n.fs m n.qs) in
  add_to_final_states (nn.q0 nn.ss nn.qs nn.ts nn.fs m.fs nn.qs)
