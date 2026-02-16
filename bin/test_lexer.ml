open Legible.Lexer

let string_of_token_type = function
  | At_chunk       -> "At_chunk"
  | At_root        -> "At_root"
  | At_end         -> "At_end"
  | At_annotation  -> "At_annotation"
  | At_comment     -> "At_comment"
  | At_ref_start   -> "At_ref_start"
  | Lbrace         -> "Lbrace"
  | Rbrace         -> "Rbrace"
  | Lbracket       -> "Lbracket"
  | Rbracket       -> "Rbracket"
  | Comma          -> "Comma"
  | Equal          -> "Equal"
  | Newline        -> "Newline"
  | Identifier s   -> Printf.sprintf "Identifier(%S)" s
  | Text s         -> Printf.sprintf "Text(%S)" s
  | Eof            -> "Eof"

let print_token tok =
  Printf.printf "%3d:%-3d  %s\n"
    tok.span.start_pos.line
    tok.span.start_pos.column
    (string_of_token_type tok.kind)

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1)
             else (print_endline "usage: ocaml test_lex.ml <file.lit>"; exit 1) in
  let ic  = open_in path in
  let src = In_channel.input_all ic in
  close_in ic;
  match tokenize src with
  | toks ->
    List.iter print_token toks
  | exception Lex_error (msg, pos) ->
    Printf.eprintf "lex error at %d:%d — %s\n" pos.line pos.column msg;
    exit 1
