open Legible
open Alcotest

let find_chunk_exn tbl name =
  match Symbol_table.find name tbl with
  | Some c ->
      c
  | None ->
      fail ("missing chunk: " ^ name)

let test_group_additive_defs_and_attach_annotations () =
  let src =
    "@annotation{once}\n\
     @chunk{a}[lang=ocaml]\n\
     one\n\
     @end\n\
     @chunk{a}\n\
     two\n\
     @end\n"
  in
  let result = src |> Parser.parse_string |> Symbol_table.build in
  let chunk = find_chunk_exn (Symbol_table.chunks result) "a" in
  check int "definition count" 2 (List.length chunk.definitions) ;
  check int "bodies count" 2 (List.length chunk.bodies) ;
  check int "chunk annotation count" 1 (List.length chunk.annotations) ;
  begin match chunk.definitions with
  | [d1; d2] ->
      check int "first def line" 2 d1.loc.line ;
      check int "second def line" 5 d2.loc.line ;
      check int "first def attached annotation count" 1
        (List.length d1.annotations) ;
      check int "second def attached annotation count" 0
        (List.length d2.annotations)
  | _ ->
      fail "expected exactly two grouped definitions"
  end ;
  check int "document annotation count" 0
    (List.length (Symbol_table.document_annotations result))

let test_unattached_annotations_become_document_level () =
  let src =
    "@annotation{doc}\n\nprose line\n@chunk{x}\nbody\n@end\n@annotation{tail}\n"
  in
  let result = src |> Parser.parse_string |> Symbol_table.build in
  let chunk = find_chunk_exn (Symbol_table.chunks result) "x" in
  check int "chunk annotation count" 0 (List.length chunk.annotations) ;
  check int "doc annotations count" 2
    (List.length (Symbol_table.document_annotations result))

let () =
  run "Legible Symbol Table Tests"
    [ ( "symbol_table"
      , [ test_case "group additive defs + attach annotations" `Quick
            test_group_additive_defs_and_attach_annotations
        ; test_case "unattached annotations are document-level" `Quick
            test_unattached_annotations_become_document_level ] ) ]
