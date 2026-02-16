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

let throw st msg = raise (Parse_error (msg, (peek st).span.start_pos))

let expect st expected msg =
  match peek_k st with
  | k when k = expected ->
      ignore (adv st)
  | _ ->
      throw st msg

let expect_lbrace st = expect st Lbrace "expected '{'"

let expect_rbrace st = expect st Rbrace "expected '}'"

let expect_lbracket st = expect st Lbracket "expected '['"

let expect_rbracket st = expect st Rbracket "expected ']'"

let expect_newline st = expect st Newline "expected newline"

let parse_identifier st p =
  match peek_k st with
  | Identifier s ->
      ignore (adv st) ;
      s
  | _ ->
      throw st ("expected identifier for " ^ p)

let parse_value st =
  match peek_k st with
  | Text s ->
      ignore (adv st) ;
      s
  | Identifier s ->
      ignore (adv st) ;
      s
  | _ ->
      throw st "expected option value"

let parse_options st =
  match peek_k st with
  | Lbracket ->
      expect_lbracket st ;
      let rec loop acc =
        match peek_k st with
        | Rbracket ->
            expect_rbracket st ; List.rev acc
        | _ -> (
            let k = parse_identifier st "option key" in
            let v =
              match peek_k st with
              | Equal ->
                  ignore (adv st) ;
                  parse_value st
              | _ ->
                  ""
            in
            let acc = (k, v) :: acc in
            match peek_k st with
            | Comma ->
                ignore (adv st) ;
                loop acc
            | Rbracket ->
                expect_rbracket st ; List.rev acc
            | _ ->
                throw st "expected ',' or ']' in options" )
      in
      loop []
  | _ ->
      []

let parse_fragment_list st ~stop_at =
  let rec loop acc =
    if stop_at (peek_k st) then List.rev acc
    else
      match peek_k st with
      | At_ref_start ->
          let ref_tok = adv st in
          let name = parse_identifier st "chunk reference" in
          expect_rbrace st ;
          loop (Ref (name, ref_tok.span.start_pos.column) :: acc)
      | Text s ->
          ignore (adv st) ;
          loop (Text s :: acc)
      | Newline ->
          ignore (adv st) ;
          loop (Text "\n" :: acc)
      | _ ->
          throw st "unexpected token"
  in
  loop []

let parse_prose st =
  parse_fragment_list st ~stop_at:(function
    | At_chunk | At_root | At_annotation | At_comment | Eof ->
        true
    | _ ->
        false )

let parse_annotation st =
  let head = adv st in
  expect_lbrace st ;
  let name = parse_identifier st "annotation name" in
  expect_rbrace st ;
  let args = parse_options st in
  ( match peek_k st with
  | Newline ->
      expect_newline st
  | Eof ->
      ()
  | _ ->
      throw st "expected newline after annotation header" ) ;
  Annotation {name; args; loc= head.span.start_pos}

let parse_chunk st is_root =
  let head = adv st in
  expect_lbrace st ;
  let name = parse_identifier st "chunk name" in
  expect_rbrace st ;
  let options = parse_options st in
  expect_newline st ;
  let body =
    parse_fragment_list st ~stop_at:(function
      | At_end | Eof ->
          true
      | _ ->
          false )
  in
  ( match peek_k st with
  | At_end ->
      ignore (adv st)
  | Eof ->
      throw st ("missing @end for chunk {" ^ name ^ "}")
  | _ ->
      throw st "expected @end" ) ;
  ChunkDef {name; is_root; options; body; loc= head.span.start_pos}

let parse_document tks =
  let st = mk_state tks in
  let rec loop acc =
    match peek_k st with
    | Eof ->
        List.rev acc
    | At_comment ->
        ignore (adv st) ;
        loop acc
    | At_annotation ->
        let item = parse_annotation st in
        loop (item :: acc)
    | At_chunk ->
        let item = parse_chunk st false in
        loop (item :: acc)
    | At_root ->
        let item = parse_chunk st true in
        loop (item :: acc)
    | Text _ | Newline | At_ref_start ->
        let prose = parse_prose st in
        if prose = [] then loop acc else loop (Prose prose :: acc)
    | At_end ->
        throw st "unexpected @end"
    | _ ->
        throw st "unexpected top level token"
  in
  loop []

let parse_string src = parse_document (tokenize src)
