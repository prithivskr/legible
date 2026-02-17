module String_map = Map.Make (String)

type t =
  {lit_hash: string; chunk_hashes: string String_map.t; out_hashes: string String_map.t; build_success: bool String_map.t}

let version = 1

let empty lit_hash =
  {lit_hash; chunk_hashes= String_map.empty; out_hashes= String_map.empty; build_success= String_map.empty}

let cache_path lit_file = Filename.concat (Filename.dirname lit_file) ".lit-cache"

let hash_string s = Digest.to_hex (Digest.string s)

let hash_file path =
  if Sys.file_exists path && not (Sys.is_directory path) then
    Some (Digest.to_hex (Digest.file path))
  else None

let find_opt m k = String_map.find_opt k m

let bool_of_string_opt = function
  | "true" ->
      Some true
  | "false" ->
      Some false
  | _ ->
      None

let parse_line (acc : t) line =
  let parts =
    line |> String.split_on_char ' ' |> List.filter (fun x -> x <> "")
  in
  match parts with
  | ["LIT_HASH"; h] ->
      {acc with lit_hash= h}
  | ["CHUNK_HASH"; name; h] ->
      {acc with chunk_hashes= String_map.add name h acc.chunk_hashes}
  | ["OUT_HASH"; path; h] ->
      {acc with out_hashes= String_map.add path h acc.out_hashes}
  | ["BUILD_OK"; path; b] -> (
      match bool_of_string_opt b with
      | Some ok ->
          {acc with build_success= String_map.add path ok acc.build_success}
      | None ->
          acc )
  | _ ->
      acc

let load path =
  if not (Sys.file_exists path) then None
  else
    let ic = open_in_bin path in
    let rec loop init =
      match input_line ic with
      | line ->
          if String.trim line = "" then loop init
          else if String.starts_with ~prefix:"VERSION " line then
            loop init
          else loop (parse_line init line)
      | exception End_of_file ->
          close_in ic ;
          Some init
    in
    loop (empty "")

let save path (c : t) =
  let oc = open_out_bin path in
  output_string oc (Printf.sprintf "VERSION %d\n" version) ;
  output_string oc ("LIT_HASH " ^ c.lit_hash ^ "\n") ;
  String_map.iter
    (fun k v -> output_string oc (Printf.sprintf "CHUNK_HASH %s %s\n" k v))
    c.chunk_hashes ;
  String_map.iter
    (fun k v -> output_string oc (Printf.sprintf "OUT_HASH %s %s\n" k v))
    c.out_hashes ;
  String_map.iter
    (fun k v -> output_string oc (Printf.sprintf "BUILD_OK %s %b\n" k v))
    c.build_success ;
  close_out oc

let is_root_stale ~cache (out : Tangle.output) =
  let name = out.path in
  let expanded_hash = hash_string out.content in
  match find_opt cache.chunk_hashes name with
  | None ->
      true
  | Some old_chunk_hash when old_chunk_hash <> expanded_hash ->
      true
  | Some _ -> (
      match (find_opt cache.out_hashes name, hash_file name) with
      | Some prev_disk, Some cur_disk when prev_disk = cur_disk -> (
          match find_opt cache.build_success name with
          | Some false ->
              true
          | _ ->
              false )
      | _ ->
          true )

let stale_roots ~force ~lit_hash ~outputs cached =
  if force then List.map (fun (o : Tangle.output) -> o.path) outputs
  else
    match cached with
    | None ->
        List.map (fun (o : Tangle.output) -> o.path) outputs
    | Some c when c.lit_hash <> lit_hash ->
        List.map (fun (o : Tangle.output) -> o.path) outputs
    | Some c ->
        outputs
        |> List.filter (is_root_stale ~cache:c)
        |> List.map (fun (o : Tangle.output) -> o.path)

let with_outputs ~lit_hash outputs cached =
  let base =
    match cached with Some c -> c | None -> empty lit_hash
  in
  let c = {base with lit_hash} in
  List.fold_left
    (fun acc (o : Tangle.output) ->
      let chunk_hashes =
        String_map.add o.path (hash_string o.content) acc.chunk_hashes
      in
      let out_hashes =
        match hash_file o.path with
        | Some h ->
            String_map.add o.path h acc.out_hashes
        | None ->
            acc.out_hashes
      in
      {acc with chunk_hashes; out_hashes})
    c outputs

let mark_build (c : t) ~root ~ok =
  {c with build_success= String_map.add root ok c.build_success}
