(* Regular Expression Type *)

type t =
  | Empty
  | Char of char
  | Union of t * t
  | Concat of t * t
  | Star of t

(* Utility *)

(* Makes a fresh state number. *)
let fresh =
  let x = ref (-1) in
  fun () -> x := !x + 1; !x

(* Part 3: Regexp *)

(* Converts a regular expression to an NFA with Thompson's construction. *)
let rec nfa_of_regex (r : t) : ('q, 's) Nfa.t =
  match r with
  | Empty -> let f1 = fresh () in let f2 = fresh() in
      {
        qs = [f1; f2];
        ss = [];
        ts = [(f1, None, f2)];
        q0 = f1;
        fs = [f2];
      }
  | Char c ->
      let f1 = fresh () in let f2 = fresh () in
      {
        qs = [f1; f2];
        ss = [c];
        ts = [(f1, Some c, f2)];
        q0 = f1;
        fs = [f2];
      }
  | Union (u, v) ->
      let f1 = fresh () in
      let f2 = fresh () in
      let unfa = (nfa_of_regex u) in
      let vnfa = (nfa_of_regex v) in
      {
        qs = unfa.qs @ vnfa.qs @ [f1; f2];
        ss = unfa.ss @ vnfa.ss;
        ts = unfa.ts @ vnfa.ts @ [(f1, None, unfa.q0); (f1, None, vnfa.q0); (List.hd unfa.fs, None, f2); (List.hd vnfa.fs, None, f2)];
        q0 = f1;
        fs = [f2];
      }
  | Concat (u, v) ->
      let f1 = fresh () in
      let f2 = fresh () in
      let unfa = (nfa_of_regex u) in
      let vnfa = (nfa_of_regex v) in
      {
        qs = unfa.qs @ vnfa.qs @ [f1; f2];
        ss = unfa.ss @ vnfa.ss;
        ts = unfa.ts @ vnfa.ts @ [(List.hd unfa.fs, None, vnfa.q0); (f1, None, unfa.q0); (List.hd vnfa.fs, None, f2)];
        q0 = f1;
        fs = [f2];
      }
  | Star r ->
      let f1 = fresh () in
      let f2 = fresh () in
      let rnfa = (nfa_of_regex r) in
      {
        qs = rnfa.qs @ [f1; f2];
        ss = rnfa.ss;
        ts = rnfa.ts @ [(f1, None, rnfa.q0); (f1, None, f2); (List.hd rnfa.fs, None, f2); (List.hd rnfa.fs, None, rnfa.q0)];
        q0 = f1;
        fs = [f2];
      }


(* Converts a regular expression to a string. *)
let rec string_of_regex (r : t) : string =
  paren (match r with
   | Empty -> "E"
   | Char c -> String.make 1 c
   | Union (u, v) -> (string_of_regex u) ^ "|" ^ (string_of_regex v)
   | Concat (u, v) -> (string_of_regex u) ^ (string_of_regex v)
   | Star r -> (string_of_regex r) ^ "*")

and paren s = "(" ^ s ^ ")"
