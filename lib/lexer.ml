type position = {
  offset : int;
  line : int;
  column : int;
}

type span = {
  start_pos : position;
  end_pos : position;
}

type token_type =
  | At_chunk
  | At_root
  | At_end
  | At_annotation
  | At_comment
  | At_ref_start
  | Lbrace
  | Rbrace
  | Lbracket
  | Rbracket
  | Comma
  | Equal
  | Newline
  | Identifier of string
  | Text of string
  | Eof

type token = {
  kind : token_type;
  span : span;
}

type t = {
  src : string;
  len : int;
  mutable i : int;
  mutable line : int;
  mutable column : int;
}

exception Lex_error of string * position


