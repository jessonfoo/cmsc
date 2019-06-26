open P3
open Regex
open Sets

(* Utility *)

(* Makes a fresh state number. *)
let fresh =
  let x = ref (-1) in
  fun () -> x := !x + 1; !x

(* Converts an extended regular expression to an NFA. *)
let rec nfa_of_regex (r : t) : (int, char) Nfa.t =
  match r with
  | Maybe r -> nfa_of_regex (Union (r, Empty))
  | Plus r -> nfa_of_regex (Concat (r, Star r))
  | Chars (c :: ct) -> nfa_of_regex (List.fold_left (fun acc x -> Union (Char x, acc)) (Char c) ct)
  | Not r -> let dfa = normalize_states(Nfa.dfa_of_nfa(nfa_of_regex r)) in
    {
      qs = dfa.qs;
      ss = dfa.ss;
      ts = dfa.ts;
      q0 = dfa.q0;
      fs = minus dfa.qs dfa.fs;
    }
  | _ -> nfa_of_regex_open r nfa_of_regex

(* Converts a generic NFA to an NFA where the states are
   integers. *)
and normalize_states (m : ('q, 's) Nfa.t) : (int, 's) Nfa.t =
  let f = Hashtbl.hash in
  { qs = List.map f m.qs;
    ss = m.ss;
    ts = List.map (fun (x, s, y) -> (f x, s, f y)) m.ts;
    q0 = f m.q0;
    fs = List.map f m.fs
  }
