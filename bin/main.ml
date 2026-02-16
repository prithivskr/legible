open Cmdliner

let run_tangle output force dry_run warn_only platform no_cache color file =
  ignore output;
  ignore force;
  ignore dry_run;
  ignore warn_only;
  ignore platform;
  ignore no_cache;
  ignore color;
  print_endline ("running tangle for " ^ file)

let run_build output force dry_run warn_only platform no_cache color file =
  ignore output;
  ignore force;
  ignore dry_run;
  ignore warn_only;
  ignore platform;
  ignore no_cache;
  ignore color;
  print_endline ("running build for " ^ file)

let run_weave output force dry_run warn_only platform offline no_cache color file =
  ignore output;
  ignore force;
  ignore dry_run;
  ignore warn_only;
  ignore platform;
  ignore offline;
  ignore no_cache;
  ignore color;
  print_endline ("running weave for " ^ file)

let run_check warn_only platform color file =
  ignore warn_only;
  ignore platform;
  ignore color;
  print_endline ("running check for " ^ file)

let run_graph color file =
  ignore color;
  print_endline ("running graph for " ^ file)

let run_clean dry_run color file =
  ignore dry_run;
  ignore color;
  print_endline ("running clean for " ^ file)

let file_arg =
  let doc = "Input literate source file." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE.lit" ~doc)

let output_arg =
  let doc = "Output path or directory." in
  Arg.(value & opt (some string) None & info [ "o" ] ~docv:"PATH" ~doc)

let force_flag =
  let doc = "Ignore cache and rebuild everything." in
  Arg.(value & flag & info [ "force" ] ~doc)

let dry_run_flag =
  let doc = "Show what would be done without writing or running commands." in
  Arg.(value & flag & info [ "dry-run" ] ~doc)

let warn_only_flag =
  let doc = "Treat annotation violations as warnings." in
  Arg.(value & flag & info [ "warn-only" ] ~doc)

let platform_opt =
  let doc = "Override host platform annotation checks." in
  Arg.(value & opt (some string) None & info [ "platform" ] ~docv:"PLATFORM" ~doc)

let offline_flag =
  let doc = "Inline assets for offline weave output." in
  Arg.(value & flag & info [ "offline" ] ~doc)

let no_cache_flag =
  let doc = "Do not read or write .lit-cache." in
  Arg.(value & flag & info [ "no-cache" ] ~doc)

let color_flag =
  let doc = "Force ANSI color output." in
  Arg.(value & flag & info [ "color" ] ~doc)

let tangle_term =
  Term.(
    const run_tangle
    $ output_arg
    $ force_flag
    $ dry_run_flag
    $ warn_only_flag
    $ platform_opt
    $ no_cache_flag
    $ color_flag
    $ file_arg)

let build_term =
  Term.(
    const run_build
    $ output_arg
    $ force_flag
    $ dry_run_flag
    $ warn_only_flag
    $ platform_opt
    $ no_cache_flag
    $ color_flag
    $ file_arg)

let weave_term =
  Term.(
    const run_weave
    $ output_arg
    $ force_flag
    $ dry_run_flag
    $ warn_only_flag
    $ platform_opt
    $ offline_flag
    $ no_cache_flag
    $ color_flag
    $ file_arg)

let check_term = Term.(const run_check $ warn_only_flag $ platform_opt $ color_flag $ file_arg)
let graph_term = Term.(const run_graph $ color_flag $ file_arg)
let clean_term = Term.(const run_clean $ dry_run_flag $ color_flag $ file_arg)

let tangle_info =
  Cmd.info "tangle" ~doc:"Expand chunks and write output files."

let build_info =
  Cmd.info "build" ~doc:"Tangle, then execute build and run commands."

let weave_info =
  Cmd.info "weave" ~doc:"Render an HTML view of a literate source."

let check_info =
  Cmd.info "check" ~doc:"Parse and validate annotations and graph structure."

let graph_info =
  Cmd.info "graph" ~doc:"Print the chunk dependency graph."

let clean_info =
  Cmd.info "clean" ~doc:"Delete generated outputs and cache metadata."

let tangle_cmd = Cmd.v tangle_info tangle_term
let build_cmd = Cmd.v build_info build_term
let weave_cmd = Cmd.v weave_info weave_term
let check_cmd = Cmd.v check_info check_term
let graph_cmd = Cmd.v graph_info graph_term
let clean_cmd = Cmd.v clean_info clean_term

let default_info = Cmd.info "legible" ~doc:"Literate programming toolchain."

let known_commands = [ "tangle"; "build"; "weave"; "check"; "status"; "graph"; "clean" ]

let is_help_flag arg =
  arg = "-h" || arg = "--help"

let rec first_non_option = function
  | [] -> None
  | "--" :: rest -> (
      match rest with
      | x :: _ -> Some x
      | [] -> None)
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
      | Some x when List.mem x known_commands -> argv
      | Some _ -> Array.of_list (prog :: "tangle" :: args)
      | None -> argv

let main_cmd =
  Cmd.group default_info [ tangle_cmd; build_cmd; weave_cmd; check_cmd; graph_cmd; clean_cmd ]

let () = exit (Cmd.eval ~argv:(argv_default Sys.argv) main_cmd)
