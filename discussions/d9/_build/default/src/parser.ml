open Lexer

(* Types *)
type var = string
type expr =
  Fun of var * expr
| App of expr * expr
| Var of var

(* Parsing helpers *)

let tok_list = ref []

(* Returns next token in the list. *)
let lookahead () : token =
  match !tok_list with
    [] -> raise (Failure "no tokens")
  | (h::t) -> h

(* Matches the top token in the list. *)
let consume (a : token) : unit =
  match !tok_list with
    (h::t) when a = h -> tok_list := t
  | _ -> raise (Failure "bad match")

(* Parses a token list. *)
let rec parse (toks : token list) : expr =
  tok_list := toks;
  let exp = parse_M () in
  if !tok_list <> [Tok_EOF] then
    raise (Failure "did not reach EOF")
  else
    exp

and parse_T' () : expr =
  match lookahead () with
  | Tok_Var _ | Tok_LParen ->
      let m = parse_M () in 
      let m2 = parse_M () in
      consume (Tok_RParen);
      App (m, m2)
  | Tok_Fun ->
      consume (Tok_Fun);
      let (Var m) = parse_X () in
      consume (Tok_Arrow);
      let n = parse_M () in
      consume Tok_RParen;
      Fun (m,n)
  | _ -> raise (Failure "parse_T' failure")


(* Parses the M (term) rule. *)
and parse_M () : expr =
  match lookahead () with
  | Tok_LParen -> 
      consume (Tok_LParen);
      parse_T' ()
  | Tok_Var x ->
      parse_X ()
  | _ -> raise (Failure "parse_M failure")

(* Parses the X (variable) rule. *)
and parse_X () : expr =
  match lookahead () with
  |Tok_Var blah -> consume (Tok_Var blah); (Var blah)
  | _ -> raise (Failure "parse_X failure")

(* Returns string representation of the AST. *)
let rec string_of_expr (m : expr) : string =
  match m with
  | Fun (v, i) -> v ^ (string_of_expr i)
  | App (m, n) -> (string_of_expr m) ^ (string_of_expr n)
  | Var vr -> vr
