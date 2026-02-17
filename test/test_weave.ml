open Legible
open Alcotest

let contains s ne =
  let slen = String.length s in
  let nlen = String.length ne in
  let rec loop i =
    if i + nlen > slen then false
    else if String.sub s i nlen = ne then true
    else loop (i + 1)
  in
  loop 0

let test_render_document_classes () =
  let src =
    "Prose\n@annotation{once}\n@chunk{x}[lang=ocaml]\nlet v = @{y}\n@end\n"
  in
  let html = src |> Parser.parse_string |> Weave.render_document in
  check bool "prose class present" true (contains html "class=\"prose-block\"") ;
  check bool "annotation class present" true
    (contains html "class=\"annotation-block\"") ;
  check bool "chunk class present" true
    (contains html "class=\"chunk-block fragment\"") ;
  check bool "ref class present" true (contains html "class=\"chunk-ref\"")

let test_render_escaping () =
  let src = "@chunk{x}\n<&>\n@end\n" in
  let html = src |> Parser.parse_string |> Weave.render_document in
  check bool "lt escaped" true (contains html "&lt;") ;
  check bool "amp escaped" true (contains html "&amp;") ;
  check bool "gt escaped" true (contains html "&gt;")

let test_prose_elements () =
  let src = "# Title\n---\nBody line\n" in
  let html = src |> Parser.parse_string |> Weave.render_document in
  check bool "h1 rendered" true
    (contains html "<h1 class=\"prose-h1\">Title</h1>") ;
  check bool "hr rendered" true (contains html "<hr class=\"prose-hr\"/>") ;
  check bool "paragraph rendered" true
    (contains html "<p class=\"prose-p\">Body line</p>")

let () =
  run "Legible Weave Tests"
    [ ( "weave"
      , [ test_case "classes are attached" `Quick test_render_document_classes
        ; test_case "html escaping" `Quick test_render_escaping
        ; test_case "prose element mapping" `Quick test_prose_elements ] ) ]
