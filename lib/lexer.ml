type position = {offset: int; line: int; column: int}

type span = {start_pos: position; end_pos: position}

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

type token = {kind: token_type; span: span}

type t =
  {src: string; len: int; mutable i: int; mutable line: int; mutable column: int}

exception Lex_error of string * position

let mk_lexer src = {src; len= String.length src; i= 0; line= 1; column= 0}

let current_pos lx = {offset= lx.i; line= lx.line; column= lx.column}

let is_at_end lx = lx.i >= lx.len

let peek lx = if is_at_end lx then '\x00' else lx.src.[lx.i]

let peek_at lx n =
  let j = lx.i + n in
  if j >= lx.len then '\x00' else lx.src.[j]

let advance lx =
  if is_at_end lx then '\x00'
  else begin
    let c = lx.src.[lx.i] in
    lx.i <- lx.i + 1 ;
    if c = '\n' then (
      lx.line <- lx.line + 1 ;
      lx.column <- 0 )
    else lx.column <- lx.column + 1 ;
    c
  end

let skip lx = ignore (advance lx)

let advance_n lx n =
  for _ = 1 to n do
    skip lx
  done

let read_while lx pred =
  let buf = Buffer.create 16 in
  while (not (is_at_end lx)) && pred (peek lx) do
    Buffer.add_char buf (advance lx)
  done ;
  Buffer.contents buf

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' | '/' ->
      true
  | _ ->
      false

let make_token kind start_pos end_pos = {kind; span= {start_pos; end_pos}}

let is_at_kword lx kw =
  let klen = String.length kw in
  let after = lx.i + 1 + klen in
  if after > lx.len then false
  else
    let rec match_chars j =
      if j >= klen then true
      else if lx.src.[lx.i + 1 + j] = kw.[j] then match_chars (j + 1)
      else false
    in
    match_chars 0 && (after = lx.len || not (is_ident_char lx.src.[after]))

type mode = Prose | Header | Body

type header_kind = Chunk | Root | Annotation

let tokenize src =
  let lx = mk_lexer src in
  let toks = ref [] in
  let mode = ref Prose in
  let hkind = ref Chunk in
  let push tok = toks := tok :: !toks in
  let emit kind start_pos = push (make_token kind start_pos (current_pos lx)) in
  let text_buf = Buffer.create 64 in
  let text_start = ref (current_pos lx) in
  let flush_text () =
    let s = Buffer.contents text_buf in
    if String.length s > 0 then begin
      push (make_token (Text s) !text_start (current_pos lx)) ;
      Buffer.clear text_buf
    end
  in
  let append c pos =
    if Buffer.length text_buf = 0 then text_start := pos ;
    Buffer.add_char text_buf c
  in
  while not (is_at_end lx) do
    let pos = current_pos lx in
    match !mode with
    | Header -> (
      match peek lx with
      | ' ' | '\t' ->
          skip lx
      | '\n' -> (
          skip lx ;
          emit Newline pos ;
          match !hkind with
          | Chunk | Root ->
              mode := Body
          | Annotation ->
              mode := Prose )
      | '{' ->
          skip lx ; emit Lbrace pos
      | '}' ->
          skip lx ; emit Rbrace pos
      | '[' ->
          skip lx ; emit Lbracket pos
      | ']' ->
          skip lx ; emit Rbracket pos
      | ',' ->
          skip lx ; emit Comma pos
      | '=' ->
          skip lx ;
          emit Equal pos ;
          let vpos = current_pos lx in
          let buf = Buffer.create 16 in
          while
            (not (is_at_end lx))
            && peek lx <> ','
            && peek lx <> ']'
            && peek lx <> '}'
            && peek lx <> '\n'
          do
            Buffer.add_char buf (advance lx)
          done ;
          let v = String.trim (Buffer.contents buf) in
          if String.length v > 0 then
            push (make_token (Text v) vpos (current_pos lx))
      | c when is_ident_char c ->
          let name = read_while lx is_ident_char in
          push (make_token (Identifier name) pos (current_pos lx))
      | c ->
          raise
            (Lex_error
               (Printf.sprintf "unexpected character %C in chunk header" c, pos)
            ) )
    | Prose | Body -> (
        let at_start = lx.column = 0 in
        match peek lx with
        | '@' when at_start && is_at_kword lx "chunk" ->
            flush_text () ;
            advance_n lx 6 ;
            emit At_chunk pos ;
            mode := Header ;
            hkind := Chunk
        | '@' when at_start && is_at_kword lx "root" ->
            flush_text () ;
            advance_n lx 5 ;
            emit At_root pos ;
            mode := Header ;
            hkind := Root
        | '@' when at_start && is_at_kword lx "end" ->
            flush_text () ;
            advance_n lx 4 ;
            emit At_end pos ;
            mode := Prose
        | '@' when at_start && is_at_kword lx "annotation" ->
            flush_text () ;
            advance_n lx 11 ;
            emit At_annotation pos ;
            mode := Header ;
            hkind := Annotation
        | '@' when at_start && peek_at lx 1 = '-' && peek_at lx 2 = '-' ->
            flush_text () ;
            while (not (is_at_end lx)) && peek lx <> '\n' do
              skip lx
            done ;
            emit At_comment pos
        | '@' when peek_at lx 1 = '{' ->
            flush_text () ;
            advance_n lx 2 ;
            emit At_ref_start pos ;
            let name_pos = current_pos lx in
            let raw = read_while lx (fun c -> c <> '}' && c <> '\n') in
            let name = String.trim raw in
            if String.length name = 0 then
              raise (Lex_error ("empty chunk reference", name_pos)) ;
            push (make_token (Identifier name) name_pos (current_pos lx)) ;
            if peek lx = '}' then begin
              let rpos = current_pos lx in
              skip lx ;
              push (make_token Rbrace rpos (current_pos lx))
            end
            else
              raise
                (Lex_error
                   ("unterminated chunk reference: expected '}'", current_pos lx)
                )
        | '\n' ->
            flush_text () ; skip lx ; emit Newline pos
        | c ->
            append c pos ; skip lx )
  done ;
  flush_text () ;
  let eof_pos = current_pos lx in
  push (make_token Eof eof_pos eof_pos) ;
  List.rev !toks
