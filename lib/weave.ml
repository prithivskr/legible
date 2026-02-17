open Ast

let escape_html s =
  let b = Buffer.create (String.length s + 32) in
  String.iter
    (function
      | '&' ->
          Buffer.add_string b "&amp;"
      | '<' ->
          Buffer.add_string b "&lt;"
      | '>' ->
          Buffer.add_string b "&gt;"
      | '"' ->
          Buffer.add_string b "&quot;"
      | '\'' ->
          Buffer.add_string b "&#39;"
      | c ->
          Buffer.add_char b c )
    s ;
  Buffer.contents b

let render_attrs attrs =
  attrs
  |> List.map (fun (k, v) -> k ^ "=\"" ^ escape_html v ^ "\"")
  |> String.concat " "

let tag ?(attrs = []) name body =
  let attrs = if attrs = [] then "" else " " ^ render_attrs attrs in
  "<" ^ name ^ attrs ^ ">" ^ body ^ "</" ^ name ^ ">"

let render_fragments frags =
  let one = function
    | Text s ->
        escape_html s
    | Ref (name, _) ->
        tag "span"
          ~attrs:[("class", "chunk-ref"); ("data-target", name)]
          ("@{" ^ escape_html name ^ "}")
  in
  frags |> List.map one |> String.concat ""

let render_options options =
  if options = [] then ""
  else
    options
    |> List.map (fun (k, v) ->
        tag "li"
          ~attrs:[("class", "chunk-option"); ("data-key", k)]
          ( tag "span" ~attrs:[("class", "option-key")] (escape_html k)
          ^ tag "span" ~attrs:[("class", "option-value")] (escape_html v) ) )
    |> String.concat ""
    |> tag "ul" ~attrs:[("class", "chunk-options")]

let render_annotation (a : annotation) =
  let args = render_options a.args in
  tag "section"
    ~attrs:[("class", "annotation-block")]
    ( tag "div"
        ~attrs:[("class", "annotation-header")]
        (tag "span" ~attrs:[("class", "annotation-name")] (escape_html a.name))
    ^ args )

let render_chunk (c : chunk_def) =
  let kind = if c.is_root then "root" else "fragment" in
  let body_html =
    tag "pre"
      ~attrs:[("class", "chunk-body")]
      (tag "code" ~attrs:[("class", "chunk-code")] (render_fragments c.body))
  in
  tag "section"
    ~attrs:
      [ ("class", "chunk-block " ^ kind)
      ; ("data-name", c.name)
      ; ("data-kind", kind) ]
    ( tag "div"
        ~attrs:[("class", "chunk-header")]
        ( tag "span" ~attrs:[("class", "chunk-kind")] kind
        ^ tag "span" ~attrs:[("class", "chunk-name")] (escape_html c.name) )
    ^ render_options c.options ^ body_html )

let render_prose frags =
  let raw =
    frags
    |> List.map (function Text s -> s | Ref (name, _) -> "@{" ^ name ^ "}")
    |> String.concat ""
  in
  let lines = String.split_on_char '\n' raw in
  let heading_level line =
    let rec count_hashes i =
      if i < String.length line && line.[i] = '#' then count_hashes (i + 1)
      else i
    in
    let n = count_hashes 0 in
    if n > 0 && n <= 6 && n < String.length line && line.[n] = ' ' then Some n
    else None
  in
  let render_line line =
    let trimmed = String.trim line in
    if trimmed = "" then ""
    else if trimmed = "---" then "<hr class=\"prose-hr\"/>"
    else
      match heading_level line with
      | Some n ->
          let content = String.sub line (n + 1) (String.length line - n - 1) in
          tag (Printf.sprintf "h%d" n)
            ~attrs:[("class", Printf.sprintf "prose-h%d" n)]
            (escape_html content)
      | None ->
          tag "p" ~attrs:[("class", "prose-p")] (escape_html line)
  in
  let body = lines |> List.map render_line |> String.concat "\n" in
  tag "section" ~attrs:[("class", "prose-block")] body

let render_document (doc : document) =
  let body =
    doc
    |> List.map (function
      | Prose frags ->
          render_prose frags
      | ChunkDef c ->
          render_chunk c
      | Annotation a ->
          render_annotation a )
    |> String.concat "\n"
  in
  "<!doctype html>\n"
  ^ "<html class=\"legible-html\"><head><meta charset=\"utf-8\">"
  ^ "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  ^ "<title>Legible Weave</title></head><body class=\"legible-body\">"
  ^ tag "main" ~attrs:[("class", "legible-document")] body
  ^ "</body></html>\n"

let default_output_path lit_path =
  let ext = Filename.extension lit_path in
  if ext = ".lit" then Filename.chop_extension lit_path ^ ".html"
  else lit_path ^ ".html"

let ensure_parent_dirs path =
  let rec mkdir_p dir =
    if dir = "" || dir = "." || Sys.file_exists dir then ()
    else (
      mkdir_p (Filename.dirname dir) ;
      Unix.mkdir dir 0o755 )
  in
  mkdir_p (Filename.dirname path)

let write_html path html =
  ensure_parent_dirs path ;
  let oc = open_out_bin path in
  output_string oc html ; close_out oc
