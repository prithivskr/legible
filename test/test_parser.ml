open Legible
open Alcotest

let test_parse_chunk_with_ref () =
  let src = "@chunk{main}[lang=ocaml]\nlet x = 1\n@{helper}\n@end\n" in
  match Parser.parse_string src with
  | Ast.ChunkDef def :: _ ->
      check string "chunk name" "main" def.name ;
      check bool "chunk should not be root" false def.is_root ;
      check
        (list (pair string string))
        "chunk options"
        [("lang", "ocaml")]
        def.options ;
      check int "body length" 5 (List.length def.body) ;
      begin match def.body with
      | [ Ast.Text "let x = 1"
        ; Ast.Text "\n"
        ; Ast.Text "  "
        ; Ast.Ref ("helper", 2)
        ; Ast.Text "\n" ] ->
          ()
      | _ ->
          fail "chunk body structure mismatch"
      end
  | _ ->
      fail "expected first item to be a chunk definition"

let test_parse_flat_annotation_then_chunk () =
  let src =
    "@-- ignored comment\n\
     @annotation{once}[scope=chunk]\n\
     @chunk{x}\n\
     body\n\
     @end\n"
  in
  let rec find_annotation_then_chunk = function
    | Ast.Annotation ann :: Ast.ChunkDef def :: _ ->
        Some (ann, def)
    | _ :: rest ->
        find_annotation_then_chunk rest
    | [] ->
        None
  in
  match find_annotation_then_chunk (Parser.parse_string src) with
  | Some (ann, def) ->
      check string "annotation name" "once" ann.name ;
      check
        (list (pair string string))
        "annotation args"
        [("scope", "chunk")]
        ann.args ;
      check string "chunk name" "x" def.name
  | None ->
      fail "expected annotation followed by chunk"

let test_parse_missing_end () =
  let src = "@chunk{oops}\nhello\n" in
  match Parser.parse_string src with
  | _ ->
      fail "expected parse error for missing @end"
  | exception Parser.Parse_error _ ->
      ()

let () =
  run "Legible Parser Tests"
    [ ( "parser"
      , [ test_case "parse chunk with ref" `Quick test_parse_chunk_with_ref
        ; test_case "parse flat annotation then chunk" `Quick
            test_parse_flat_annotation_then_chunk
        ; test_case "parse missing end" `Quick test_parse_missing_end ] ) ]
