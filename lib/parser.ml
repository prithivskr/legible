open Lexer
open Ast

exception Parse_error of string * position

type state = {tokens: token array; mutable pos: int}

let mk_state toks = {tokens= Array.of_list toks; pos= 0}

let fin st = st.pos >= Array.length st.tokens

let peek st =
  if fin st then
    let ep = {offset= 0; line= 0; column= 0} in
    {kind= Eof; span= {start_pos= ep; end_pos= ep}}
  else st.tokens.(st.pos)

let peek_k st = (peek st).kind

let adv st =
  let t = peek st in
  st.pos <- st.pos + 1 ;
  t

let parse_string src = [Prose [Text src]]
