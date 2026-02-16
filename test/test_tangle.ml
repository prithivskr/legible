open Legible
open Alcotest

let parse_chunks src =
  src |> Parser.parse_string |> Symbol_table.build |> Symbol_table.chunks

let find_output_exn path outs =
  match List.find_opt (fun (o : Tangle.output) -> o.path = path) outs with
  | Some o ->
      o
  | None ->
      fail ("missing output: " ^ path)

let test_recursive_expansion_with_indentation () =
  let src =
    "@chunk{leaf}\n\
     line1\n\
     line2\n\
     @end\n\
     @chunk{mid}\n\
     start\n\
    \  @{leaf}\n\
     done\n\
     @end\n\
     @root{out.txt}\n\
     header\n\
     @{mid}\n\
     @end\n"
  in
  let outs = src |> parse_chunks |> Tangle.expand in
  let out = find_output_exn "out.txt" outs in
  let expected = "header\nstart\n  line1\n  line2\ndone\n" in
  check string "expanded content" expected out.content

let test_additive_chunks_concatenate_in_order () =
  let src =
    "@chunk{a}\none\n@end\n@chunk{a}\ntwo\n@end\n@root{out.txt}\n@{a}\n@end\n"
  in
  let outs = src |> parse_chunks |> Tangle.expand in
  let out = find_output_exn "out.txt" outs in
  check string "additive body order" "one\ntwo\n" out.content

let test_undefined_ref_fails () =
  let src = "@root{out.txt}\n@{missing}\n@end\n" in
  let chunks = parse_chunks src in
  match Tangle.expand chunks with
  | _ ->
      fail "expected undefined reference failure"
  | exception Tangle.Tangle_error msg ->
      check bool "mentions undefined" true
        (String.length msg > 0 && String.contains msg 'u')

let test_write_outputs () =
  let src = "@root{sub/dir/out.txt}\nhello\n@end\n" in
  let outs = src |> parse_chunks |> Tangle.expand in
  let temp_dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "legible-tangle-%d" (Unix.getpid ()))
  in
  if not (Sys.file_exists temp_dir) then Unix.mkdir temp_dir 0o755 ;
  Tangle.write_outputs ~base_dir:temp_dir outs ;
  let path = Filename.concat temp_dir "sub/dir/out.txt" in
  let ic = open_in_bin path in
  let content = In_channel.input_all ic in
  close_in ic ;
  check string "written file content" "hello\n" content

let () =
  run "Legible Tangle Tests"
    [ ( "tangle"
      , [ test_case "recursive expansion + indentation" `Quick
            test_recursive_expansion_with_indentation
        ; test_case "additive chunks concatenate in order" `Quick
            test_additive_chunks_concatenate_in_order
        ; test_case "undefined ref fails" `Quick test_undefined_ref_fails
        ; test_case "write outputs" `Quick test_write_outputs ] ) ]
