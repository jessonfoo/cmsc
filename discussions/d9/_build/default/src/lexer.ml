module L = List
module S = String
module R =  Str

(* Type *)
type token =
  Tok_Fun
| Tok_Var of string
| Tok_Arrow
| Tok_LParen
| Tok_RParen
| Tok_EOF

let re = [
  ("fun", fun _ -> [Tok_Fun]);
  ("[a-z]+", fun x -> [Tok_Var x]);
  ("->", fun _ -> [Tok_Arrow]);
  ("(", fun _ -> [Tok_LParen]);
  (")", fun _ -> [Tok_RParen]);
  (" ", fun _ -> [])
]

(* Given source code returns a token list. *)
let rec lex (s : string) : token list =
  lex' s 0

and lex' (s: string) (pos: int): token list =
  if pos >= S.length s then [Tok_EOF]
  else
    let (_, f) = L.find (fun (re, _) -> R.string_match (R.regexp re) s pos) re in
    let s' = R.matched_string s in
    (f s') @ (lex' s (pos + (S.length s')))

(* Returns a string representation of a token list. *)
let rec string_of_tokens (ts : token list) : string =
  S.concat "" (L.map string_of_token ts)

and string_of_token (t: token) : string =
  match t with
  | Tok_Fun -> "fun"
  | Tok_Var x -> x
  | Tok_Arrow -> "->"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_EOF -> ""
