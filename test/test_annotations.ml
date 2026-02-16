open Legible
open Alcotest

let parse_symbols src = src |> Parser.parse_string |> Symbol_table.build

let parse_analysis src =
  let symbols = parse_symbols src in
  let chunks = Symbol_table.chunks symbols in
  let dag = Dag.analyze chunks in
  (symbols, dag)

let test_static_checks () =
  let src =
    "@annotation{no-additive}\n\
     @chunk{a}[lang=ocaml]\n\
     one\n\
     @end\n\
     @chunk{a}[lang=ocaml]\n\
     two\n\
     @end\n\
     @annotation{require}[lang=python]\n\
     @chunk{b}[lang=ocaml]\n\
     b\n\
     @end\n\
     @annotation{abstract}\n\
     @chunk{c}\n\
     @end\n"
  in
  let report = src |> parse_symbols |> Annotations.check_static in
  check bool "has static errors" true (Annotations.has_errors report) ;
  check int "static error count" 3 (List.length report.errors)

let test_graph_checks () =
  let src =
    "@annotation{strict-lang}\n\
     @root{main}[lang=python]\n\
     @{wrapper}\n\
     @end\n\
     @chunk{wrapper}[lang=python]\n\
     @{danger}\n\
     @{init}\n\
     @{init}\n\
     @end\n\
     @annotation{exclude-from}[lang=python]\n\
     @annotation{deprecated}[msg=use-safe]\n\
     @chunk{danger}[lang=c]\n\
     danger\n\
     @end\n\
     @annotation{max-refs}[n=1]\n\
     @chunk{init}[lang=python]\n\
     init\n\
     @end\n"
  in
  let symbols, dag = parse_analysis src in
  let report = Annotations.check_graph symbols dag in
  check bool "has graph errors" true (List.length report.errors >= 3) ;
  check bool "has graph warnings" true (List.length report.warnings >= 1)

let test_platform_check () =
  let src =
    "@root{main}[lang=python]\n\
     @{win}\n\
     @end\n\
     @annotation{platform}[value=windows]\n\
     @chunk{win}[lang=python]\n\
     x\n\
     @end\n"
  in
  let symbols, dag = parse_analysis src in
  let report = Annotations.check_platform ~host:Annotations.Posix symbols dag in
  check int "one platform warning" 1 (List.length report.warnings)

let () =
  run "Legible Annotations Tests"
    [ ( "annotations"
      , [ test_case "static checks" `Quick test_static_checks
        ; test_case "graph checks" `Quick test_graph_checks
        ; test_case "platform check" `Quick test_platform_check ] ) ]
