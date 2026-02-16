open Cmdliner
open Legible

type pipeline = {symbols: Symbol_table.build_result; dag: Dag.analysis}

let slurp path =
  let ic = open_in_bin path in
  let src = In_channel.input_all ic in
  close_in ic ; src

let pos_to_string (p : Lexer.position) = Printf.sprintf "%d:%d" p.line p.column

let diagnostic_to_string (d : Annotations.diagnostic) =
  let sev = match d.severity with Error -> "error" | Warning -> "warning" in
  let where =
    match (d.chunk, d.loc) with
    | Some name, Some loc ->
        Printf.sprintf "[%s @ %s]" name (pos_to_string loc)
    | Some name, None ->
        Printf.sprintf "[%s]" name
    | None, Some loc ->
        Printf.sprintf "[@ %s]" (pos_to_string loc)
    | None, None ->
        ""
  in
  if where = "" then Printf.sprintf "%s: %s" sev d.message
  else Printf.sprintf "%s %s: %s" sev where d.message

let print_report (r : Annotations.report) =
  List.iter (fun d -> prerr_endline (diagnostic_to_string d)) r.warnings ;
  List.iter (fun d -> prerr_endline (diagnostic_to_string d)) r.errors

let parse_host_platform override =
  match override with
  | Some s -> (
    match String.lowercase_ascii s with
    | "posix" ->
        Annotations.Posix
    | "windows" ->
        Annotations.Windows
    | other ->
        invalid_arg ("unsupported --platform value: " ^ other) )
  | None ->
      if Sys.win32 then Annotations.Windows else Annotations.Posix
let delete_path ~dry_run path =
  if Sys.file_exists path then
    if dry_run then print_endline ("would delete " ^ path)
    else if Sys.is_directory path then
      prerr_endline ("warning: skipping directory " ^ path)
    else (
      Sys.remove path ;
      print_endline ("deleted " ^ path) )
  else if dry_run then print_endline ("would skip missing " ^ path)

let run_tangle output force dry_run warn_only platform no_cache color file =
  ignore force ;
  ignore no_cache ;
  ignore color ;
  run_with_errors (fun () ->
      let pipeline = build_pipeline file in
      let ok = check_annotations ~warn_only ~platform pipeline in
      if not ok then exit 1 ;
      let outputs = Tangle.expand (Symbol_table.chunks pipeline.symbols) in
      let outputs =
        match (output, outputs) with
        | Some path, [out] ->
            [{Tangle.path; content= out.content}]
        | _ ->
            outputs
      in
      if dry_run then
        List.iter
          (fun (o : Tangle.output) ->
            print_endline
              (Printf.sprintf "would write %s (%d bytes)" o.path
                 (String.length o.content) ) )
          outputs
      else
        match (output, outputs) with
        | Some base, _ :: _ :: _ ->
            Tangle.write_outputs ~base_dir:base outputs
        | _ ->
            Tangle.write_outputs outputs )

let run_build output force dry_run warn_only platform no_cache color file =
  ignore force ;
  ignore no_cache ;
  ignore color ;
  run_with_errors (fun () ->
      let pipeline = build_pipeline file in
      let ok = check_annotations ~warn_only ~platform pipeline in
      if not ok then exit 1 ;
      let outputs = Tangle.expand (Symbol_table.chunks pipeline.symbols) in
      let outputs =
        match (output, outputs) with
        | Some path, [out] ->
            [{Tangle.path; content= out.content}]
        | _ ->
            outputs
      in
      ( if dry_run then
          List.iter
            (fun (o : Tangle.output) ->
              print_endline
                (Printf.sprintf "would write %s (%d bytes)" o.path
                   (String.length o.content) ) )
            outputs
        else
          match (output, outputs) with
          | Some base, _ :: _ :: _ ->
              Tangle.write_outputs ~base_dir:base outputs
          | _ ->
              Tangle.write_outputs outputs ) ;
      let build_result = Build.run ~dry_run pipeline.symbols in
      Build.print_result build_result ;
      if Build.has_failures build_result then exit 1 )

let run_weave output force dry_run warn_only platform offline no_cache color
    file =
  ignore output ;
  ignore force ;
  ignore dry_run ;
  ignore warn_only ;
  ignore platform ;
  ignore offline ;
  ignore no_cache ;
  ignore color ;
  ignore file ;
  prerr_endline "weave is not implemented yet" ;
  exit 1

let run_check warn_only platform color file =
  ignore color ;
  run_with_errors (fun () ->
      let pipeline = build_pipeline file in
      let ok = check_annotations ~warn_only ~platform pipeline in
      if not ok then exit 1 ;
      print_endline "check passed" )

let run_graph color file =
  ignore color ;
  run_with_errors (fun () ->
      let pipeline = build_pipeline file in
      let g = pipeline.dag.graph.adjacency in
      Dag.String_map.iter
        (fun from deps ->
          Dag.String_set.iter
            (fun dep -> print_endline (from ^ " -> " ^ dep))
            deps )
        g )

let run_clean dry_run color file =
  ignore color ;
  run_with_errors (fun () ->
      let pipeline = build_pipeline file in
      let outputs = root_output_paths pipeline.symbols in
      outputs
      |> List.iter (fun out ->
          path_candidates ~lit_file:file out |> List.iter (delete_path ~dry_run) ) ;
      let cache = Filename.concat (Filename.dirname file) ".lit-cache" in
      delete_path ~dry_run cache )

let file_arg =
  let doc = "Input literate source file." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE.lit" ~doc)

let output_arg =
  let doc = "Output path or directory." in
  Arg.(value & opt (some string) None & info ["o"] ~docv:"PATH" ~doc)

let force_flag =
  let doc = "Ignore cache and rebuild everything." in
  Arg.(value & flag & info ["force"] ~doc)

let dry_run_flag =
  let doc = "Show what would be done without writing or running commands." in
  Arg.(value & flag & info ["dry-run"] ~doc)

let warn_only_flag =
  let doc = "Treat annotation violations as warnings." in
  Arg.(value & flag & info ["warn-only"] ~doc)

let platform_opt =
  let doc = "Override host platform annotation checks." in
  Arg.(value & opt (some string) None & info ["platform"] ~docv:"PLATFORM" ~doc)

let offline_flag =
  let doc = "Inline assets for offline weave output." in
  Arg.(value & flag & info ["offline"] ~doc)

let no_cache_flag =
  let doc = "Do not read or write .lit-cache." in
  Arg.(value & flag & info ["no-cache"] ~doc)

let color_flag =
  let doc = "Force ANSI color output." in
  Arg.(value & flag & info ["color"] ~doc)

let tangle_term =
  Term.(
    const run_tangle $ output_arg $ force_flag $ dry_run_flag $ warn_only_flag
    $ platform_opt $ no_cache_flag $ color_flag $ file_arg )

let build_term =
  Term.(
    const run_build $ output_arg $ force_flag $ dry_run_flag $ warn_only_flag
    $ platform_opt $ no_cache_flag $ color_flag $ file_arg )

let weave_term =
  Term.(
    const run_weave $ output_arg $ force_flag $ dry_run_flag $ warn_only_flag
    $ platform_opt $ offline_flag $ no_cache_flag $ color_flag $ file_arg )

let check_term =
  Term.(const run_check $ warn_only_flag $ platform_opt $ color_flag $ file_arg)

let graph_term = Term.(const run_graph $ color_flag $ file_arg)

let clean_term = Term.(const run_clean $ dry_run_flag $ color_flag $ file_arg)

let tangle_info = Cmd.info "tangle" ~doc:"Expand chunks and write output files."

let build_info =
  Cmd.info "build" ~doc:"Tangle, then execute build and run commands."

let weave_info =
  Cmd.info "weave" ~doc:"Render an HTML view of a literate source."

let check_info =
  Cmd.info "check" ~doc:"Parse and validate annotations and graph structure."

let graph_info = Cmd.info "graph" ~doc:"Print the chunk dependency graph."

let clean_info =
  Cmd.info "clean" ~doc:"Delete generated outputs and cache metadata."

let tangle_cmd = Cmd.v tangle_info tangle_term

let build_cmd = Cmd.v build_info build_term

let weave_cmd = Cmd.v weave_info weave_term

let check_cmd = Cmd.v check_info check_term

let graph_cmd = Cmd.v graph_info graph_term

let clean_cmd = Cmd.v clean_info clean_term

let default_info = Cmd.info "legible" ~doc:"Literate programming toolchain."

let known_commands =
  ["tangle"; "build"; "weave"; "check"; "status"; "graph"; "clean"]

let is_help_flag arg = arg = "-h" || arg = "--help"

let rec first_non_option = function
  | [] ->
      None
  | "--" :: rest -> (
    match rest with x :: _ -> Some x | [] -> None )
  | x :: rest ->
      if String.length x > 0 && Char.equal x.[0] '-' then first_non_option rest
      else Some x

let argv_default argv =
  if Array.length argv <= 1 then argv
  else
    let prog = argv.(0) in
    let args = Array.to_list (Array.sub argv 1 (Array.length argv - 1)) in
    if List.exists is_help_flag args then argv
    else
      match first_non_option args with
      | Some x when List.mem x known_commands ->
          argv
      | Some _ ->
          Array.of_list (prog :: "tangle" :: args)
      | None ->
          argv

let main_cmd =
  Cmd.group default_info
    [tangle_cmd; build_cmd; weave_cmd; check_cmd; graph_cmd; clean_cmd]

let () = exit (Cmd.eval ~argv:(argv_default Sys.argv) main_cmd)
