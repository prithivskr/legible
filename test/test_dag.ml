open Legible
open Alcotest

let index_of name xs =
  let rec loop i = function
    | [] ->
        None
    | x :: tl ->
        if x = name then Some i else loop (i + 1) tl
  in
  loop 0 xs

let require_index name xs =
  match index_of name xs with
  | Some i ->
      i
  | None ->
      fail ("missing name in list: " ^ name)

let test_topo_order_dependencies_first () =
  let src =
    "@root{main}\n\
     @{mid}\n\
     @end\n\
     @chunk{mid}\n\
     @{leaf}\n\
     @end\n\
     @chunk{leaf}\n\
     body\n\
     @end\n"
  in
  let chunks =
    src |> Parser.parse_string |> Symbol_table.build |> Symbol_table.chunks
  in
  let res = Dag.analyze chunks in
  let i_leaf = require_index "leaf" res.topo in
  let i_mid = require_index "mid" res.topo in
  let i_main = require_index "main" res.topo in
  check bool "leaf before mid" true (i_leaf < i_mid) ;
  check bool "mid before main" true (i_mid < i_main)

let test_cycle_detection () =
  let src = "@chunk{a}\n@{b}\n@end\n@chunk{b}\n@{a}\n@end\n" in
  let chunks =
    src |> Parser.parse_string |> Symbol_table.build |> Symbol_table.chunks
  in
  match Dag.analyze chunks with
  | _ ->
      fail "expected cycle detection"
  | exception Dag.Cycle_error path ->
      check bool "non-empty cycle path" true (List.length path >= 2)

let test_unreachable_from_roots () =
  let src =
    "@root{main}\n\
     @{util}\n\
     @end\n\
     @chunk{util}\n\
     ok\n\
     @end\n\
     @chunk{dead}\n\
     noop\n\
     @end\n"
  in
  let chunks =
    src |> Parser.parse_string |> Symbol_table.build |> Symbol_table.chunks
  in
  let res = Dag.analyze chunks in
  check (list string) "unreachable chunks" ["dead"] res.unreachable

let () =
  run "Legible DAG Tests"
    [ ( "dag"
      , [ test_case "topo dependencies first" `Quick
            test_topo_order_dependencies_first
        ; test_case "cycle detection" `Quick test_cycle_detection
        ; test_case "unreachable from roots" `Quick test_unreachable_from_roots
        ] ) ]
