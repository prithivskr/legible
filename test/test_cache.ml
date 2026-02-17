open Legible
open Alcotest

let test_cache_roundtrip () =
  let path = Filename.concat (Filename.get_temp_dir_name ()) "legible-cache-test" in
  let c0 = Cache.empty "abc" in
  let c0 = Cache.mark_build c0 ~root:"out.txt" ~ok:true in
  let c0 =
    { c0 with
      chunk_hashes= Cache.String_map.add "out.txt" "h1" c0.chunk_hashes
    ; out_hashes= Cache.String_map.add "out.txt" "h2" c0.out_hashes }
  in
  Cache.save path c0 ;
  let c1 = Cache.load path in
  Sys.remove path ;
  match c1 with
  | None ->
      fail "cache should load"
  | Some c ->
      check string "lit hash" "abc" c.lit_hash ;
      check (option string) "chunk hash" (Some "h1")
        (Cache.String_map.find_opt "out.txt" c.chunk_hashes) ;
      check (option bool) "build success" (Some true)
        (Cache.String_map.find_opt "out.txt" c.build_success)

let test_stale_roots_detection () =
  let out = {Tangle.path= "cache-out.txt"; content= "hello\n"} in
  let lit_hash = Cache.hash_string "src" in
  let stale = Cache.stale_roots ~force:false ~lit_hash ~outputs:[out] None in
  check (list string) "empty cache => stale" ["cache-out.txt"] stale ;
  let oc = open_out_bin out.path in
  output_string oc out.content ;
  close_out oc ;
  let c = Cache.with_outputs ~lit_hash [out] None in
  let stale2 = Cache.stale_roots ~force:false ~lit_hash ~outputs:[out] (Some c) in
  Sys.remove out.path ;
  check (list string) "up-to-date => no stale" [] stale2

let () =
  run "Legible Cache Tests"
    [ ( "cache"
      , [ test_case "roundtrip" `Quick test_cache_roundtrip
        ; test_case "stale roots detection" `Quick test_stale_roots_detection ]
      ) ]
