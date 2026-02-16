open Ast
module String_map = Map.Make (String)
module String_set = Set.Make (String)

type severity = Error | Warning

type diagnostic =
  {severity: severity; message: string; chunk: string option; loc: loc option}

type report = {errors: diagnostic list; warnings: diagnostic list}

type host_platform = Posix | Windows

let empty = {errors= []; warnings= []}

let add_error report ?chunk ?loc message =
  {report with errors= {severity= Error; message; chunk; loc} :: report.errors}

let add_warning report ?chunk ?loc message =
  { report with
    warnings= {severity= Warning; message; chunk; loc} :: report.warnings }

let merge a b = {errors= a.errors @ b.errors; warnings= a.warnings @ b.warnings}

let has_errors report = report.errors <> []

let reverse report =
  {errors= List.rev report.errors; warnings= List.rev report.warnings}

let find_opt key kvs =
  List.find_opt (fun (k, _) -> k = key) kvs |> Option.map snd

let find_int key kvs =
  match find_opt key kvs with
  | Some s -> (
    match int_of_string_opt s with Some n -> Some n | None -> None )
  | None ->
      None

let chunk_lang (chunk : Symbol_table.chunk_def) =
  let langs =
    List.filter_map
      (fun (d : Symbol_table.definition) -> find_opt "lang" d.options)
      chunk.definitions
    |> List.sort_uniq String.compare
  in
  match langs with [] -> None | [x] -> Some x | x :: _ -> Some x

let refs_of_body body =
  List.fold_left
    (fun acc -> function Ref (name, _) -> name :: acc | Text _ -> acc)
    [] body
  |> List.rev

let refs_of_definition (d : Symbol_table.definition) = refs_of_body d.body

let refs_of_chunk (chunk : Symbol_table.chunk_def) =
  List.fold_left (fun acc d -> acc @ refs_of_definition d) [] chunk.definitions

let has_annotation anns name =
  List.exists (fun (a : annotation) -> a.name = name) anns

let annotations_named anns name =
  List.filter (fun (a : annotation) -> a.name = name) anns

let is_doc_flag symbols name =
  let in_doc =
    Symbol_table.document_annotations symbols
    |> List.exists (fun (a : annotation) -> a.name = name)
  in
  if in_doc then true
  else
    Symbol_table.chunks symbols
    |> Symbol_table.String_map.exists (fun _ (chunk : Symbol_table.chunk_def) ->
        has_annotation chunk.annotations name )

let body_has_content body =
  List.exists (function Ref _ -> true | Text s -> String.trim s <> "") body

let check_static (symbols : Symbol_table.build_result) =
  let no_additive = is_doc_flag symbols "no-additive" in
  let chunks = Symbol_table.chunks symbols in
  let report =
    Symbol_table.String_map.fold
      (fun name (chunk : Symbol_table.chunk_def) report ->
        let defs = List.length chunk.definitions in
        let chunk_loc =
          match chunk.definitions with d :: _ -> Some d.loc | [] -> None
        in
        let report =
          if defs > 1 && (no_additive || has_annotation chunk.annotations "once")
          then
            add_error report ~chunk:name ?loc:chunk_loc
              ( "chunk '" ^ name
              ^ "' has multiple definitions but is marked once" )
          else report
        in
        let report =
          if has_annotation chunk.annotations "abstract" then
            let has_non_empty =
              List.exists
                (fun (d : Symbol_table.definition) -> body_has_content d.body)
                chunk.definitions
            in
            if has_non_empty then report
            else
              add_error report ~chunk:name ?loc:chunk_loc
                ( "chunk '" ^ name
                ^ "' is abstract but has no concrete definition body" )
          else report
        in
        let report =
          List.fold_left
            (fun report (ann : annotation) ->
              if ann.name <> "require" then report
              else
                match find_opt "lang" ann.args with
                | None ->
                    add_error report ~chunk:name ~loc:ann.loc
                      "require annotation missing lang=<value>"
                | Some expected ->
                    List.fold_left
                      (fun report (d : Symbol_table.definition) ->
                        match find_opt "lang" d.options with
                        | Some actual when actual = expected ->
                            report
                        | _ ->
                            add_error report ~chunk:name ~loc:d.loc
                              ( "chunk '" ^ name ^ "' must declare lang="
                              ^ expected ) )
                      report chunk.definitions )
            report chunk.annotations
        in
        report )
      chunks empty
  in
  reverse report

let check_lang_rule report ~strict_lang chunks name
    (chunk : Symbol_table.chunk_def) =
  if strict_lang || has_annotation chunk.annotations "lang-check" then
    let owner_lang = chunk_lang chunk in
    match owner_lang with
    | None ->
        add_error report ~chunk:name
          "lang-check requires lang=<value> on source chunk"
    | Some src_lang ->
        refs_of_chunk chunk
        |> List.fold_left
             (fun report dep ->
               match Symbol_table.find dep chunks with
               | None ->
                   report
               | Some dep_chunk -> (
                 match chunk_lang dep_chunk with
                 | Some dep_lang when dep_lang = src_lang ->
                     report
                 | Some dep_lang ->
                     add_error report ~chunk:name
                       ( "lang-check violation: '" ^ name ^ "' (" ^ src_lang
                       ^ ") references '" ^ dep ^ "' (" ^ dep_lang ^ ")" )
                 | None ->
                     add_error report ~chunk:name
                       ( "lang-check violation: '" ^ name ^ "' references '"
                       ^ dep ^ "' without lang tag" ) ) )
             report
  else report

let root_langs chunks roots =
  List.filter_map
    (fun root ->
      match Symbol_table.find root chunks with
      | None ->
          None
      | Some chunk ->
          Option.map (fun lang -> (root, lang)) (chunk_lang chunk) )
    roots

let reachable_from_root (graph : Dag.t) root =
  let seen = Hashtbl.create 32 in
  let rec dfs n =
    if not (Hashtbl.mem seen n) then (
      Hashtbl.replace seen n true ;
      let deps =
        match String_map.find_opt n graph.adjacency with
        | Some s ->
            s
        | None ->
            String_set.empty
      in
      String_set.iter dfs deps )
  in
  dfs root ; seen

let check_exclude_from report chunks analysis =
  let root_lang_pairs = root_langs chunks analysis.Dag.graph.roots in
  Symbol_table.String_map.fold
    (fun name (chunk : Symbol_table.chunk_def) report ->
      let excluded_langs =
        chunk.annotations
        |> fun anns ->
        annotations_named anns "exclude-from"
        |> List.filter_map (fun ann -> find_opt "lang" ann.args)
      in
      List.fold_left
        (fun report excluded_lang ->
          List.fold_left
            (fun report (root, lang) ->
              if lang <> excluded_lang then report
              else
                let reachable = reachable_from_root analysis.Dag.graph root in
                if Hashtbl.mem reachable name then
                  add_error report ~chunk:name
                    ( "exclude-from violation: chunk '" ^ name
                    ^ "' is reachable from root '" ^ root ^ "' with lang="
                    ^ lang )
                else report )
            report root_lang_pairs )
        report excluded_langs )
    chunks report

let add_count name n counts =
  let cur =
    match String_map.find_opt name counts with Some x -> x | None -> 0
  in
  String_map.add name (cur + n) counts

let merge_counts a b = String_map.fold (fun k v acc -> add_count k v acc) b a

let check_max_refs report chunks analysis =
  let memo = Hashtbl.create 32 in
  let rec contrib name =
    match Hashtbl.find_opt memo name with
    | Some m ->
        m
    | None ->
        let m =
          match Symbol_table.find name chunks with
          | None ->
              String_map.empty
          | Some chunk ->
              refs_of_chunk chunk
              |> List.fold_left
                   (fun counts dep ->
                     let counts = add_count dep 1 counts in
                     merge_counts counts (contrib dep) )
                   String_map.empty
        in
        Hashtbl.replace memo name m ;
        m
  in
  let total =
    List.fold_left
      (fun counts root -> merge_counts counts (contrib root))
      String_map.empty analysis.Dag.graph.roots
  in
  Symbol_table.String_map.fold
    (fun name (chunk : Symbol_table.chunk_def) report ->
      let usage =
        match String_map.find_opt name total with Some x -> x | None -> 0
      in
      chunk.annotations
      |> fun anns ->
      annotations_named anns "max-refs"
      |> List.fold_left
           (fun report ann ->
             match find_int "n" ann.args with
             | Some n ->
                 if usage <= n then report
                 else
                   add_error report ~chunk:name ~loc:ann.loc
                     ( "max-refs violation: chunk '" ^ name ^ "' appears "
                     ^ string_of_int usage ^ " times (limit " ^ string_of_int n
                     ^ ")" )
             | None ->
                 add_error report ~chunk:name ~loc:ann.loc
                   "max-refs annotation missing n=<int>" )
           report )
    chunks report

let check_deprecated_warnings report chunks =
  let deprecated =
    Symbol_table.String_map.fold
      (fun name (chunk : Symbol_table.chunk_def) acc ->
        let messages =
          chunk.annotations
          |> fun anns ->
          annotations_named anns "deprecated"
          |> List.filter_map (fun ann -> find_opt "msg" ann.args)
        in
        match messages with [] -> acc | _ -> String_map.add name messages acc )
      chunks String_map.empty
  in
  Symbol_table.String_map.fold
    (fun owner (chunk : Symbol_table.chunk_def) report ->
      refs_of_chunk chunk
      |> List.fold_left
           (fun report dep ->
             match String_map.find_opt dep deprecated with
             | None ->
                 report
             | Some messages ->
                 List.fold_left
                   (fun report msg ->
                     add_warning report ~chunk:owner
                       ("reference to deprecated chunk '" ^ dep ^ "': " ^ msg) )
                   report messages )
           report )
    chunks report

let check_undefined_refs report chunks =
  Symbol_table.String_map.fold
    (fun owner (chunk : Symbol_table.chunk_def) report ->
      refs_of_chunk chunk
      |> List.fold_left
           (fun report dep ->
             if Symbol_table.mem dep chunks then report
             else
               add_error report ~chunk:owner
                 ("undefined chunk reference: '" ^ owner ^ "' -> '" ^ dep ^ "'") )
           report )
    chunks report

let check_graph (symbols : Symbol_table.build_result) (analysis : Dag.analysis)
    =
  let chunks = Symbol_table.chunks symbols in
  let strict_lang = is_doc_flag symbols "strict-lang" in
  let report =
    Symbol_table.String_map.fold
      (fun name chunk report ->
        check_lang_rule report ~strict_lang chunks name chunk )
      chunks empty
  in
  let report = check_undefined_refs report chunks in
  let report = check_exclude_from report chunks analysis in
  let report = check_max_refs report chunks analysis in
  let report = check_deprecated_warnings report chunks in
  reverse report

let host_string = function Posix -> "posix" | Windows -> "windows"

let platform_value (chunk : Symbol_table.chunk_def) =
  chunk.annotations
  |> fun anns ->
  annotations_named anns "platform"
  |> List.find_map (fun ann ->
      match find_opt "value" ann.args with
      | Some v ->
          Some (String.lowercase_ascii v)
      | None ->
          find_opt "platform" ann.args |> Option.map String.lowercase_ascii )

let check_platform ~host (symbols : Symbol_table.build_result)
    (analysis : Dag.analysis) =
  let chunks = Symbol_table.chunks symbols in
  let host = host_string host in
  let reachable =
    analysis.Dag.graph.roots
    |> List.fold_left
         (fun acc root ->
           let seen = reachable_from_root analysis.Dag.graph root in
           Hashtbl.to_seq_keys seen
           |> Seq.fold_left (fun acc k -> String_set.add k acc) acc )
         String_set.empty
  in
  let report =
    Symbol_table.String_map.fold
      (fun name chunk report ->
        if not (String_set.mem name reachable) then report
        else
          match platform_value chunk with
          | None | Some "any" ->
              report
          | Some p when p = host ->
              report
          | Some p ->
              add_warning report ~chunk:name
                ( "platform mismatch: chunk '" ^ name ^ "' is platform=" ^ p
                ^ " on host " ^ host ) )
      chunks empty
  in
  reverse report
