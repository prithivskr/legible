open Lexer
open Ast

exception Parse_error of string * position

let parse_string src = [Prose [Text src]]
