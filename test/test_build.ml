open Legible
open Alcotest

module String_map = Map.Make (String)

let parse_symbols src = src |> Parser.parse_string |> Symbol_table.build

let status_exn (r : Build.result) name =
  match String_map.find_opt name r.statuses with
  | Some s ->
      s
  | None ->
      fail ("missing status for " ^ name)

let test_build_order_from_deps () =
  let src =
    "@root{a}[build=true]\nA\n@end\n\
     @root{b}[deps=a,build=true]\nB\n@end\n\
     @root{c}[deps=b,build=true]\nC\n@end\n"
  in
  let symbols = parse_symbols src in
  let r = Build.run ~dry_run:true symbols in
  check (list string) "topo build order" ["a"; "b"; "c"] r.order ;
  check bool "no failures" false (Build.has_failures r)

let test_build_failure_propagates_to_dependents () =
  let src =
    "@root{a}[build=false]\nA\n@end\n\
     @root{b}[deps=a,build=true]\nB\n@end\n"
  in
  let symbols = parse_symbols src in
  let r = Build.run symbols in
  begin
    match status_exn r "a" with
    | Build.Failed _ ->
        ()
    | _ ->
        fail "a should fail"
  end ;
  begin
    match status_exn r "b" with
    | Build.Skipped _ ->
        ()
    | _ ->
        fail "b should be skipped due to failed dependency"
  end

let () =
  run "Legible Build Tests"
    [ ( "build"
      , [ test_case "order from deps" `Quick test_build_order_from_deps
        ; test_case "failure propagation" `Quick
            test_build_failure_propagates_to_dependents ] ) ]
