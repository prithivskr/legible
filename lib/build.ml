module String_map = Map.Make (String)
module String_set = Set.Make (String)

type root =
  { name: string
  ; build_cmd: string option
  ; run_cmd: string option
  ; deps: string list }

type root_status = Succeeded | Cached | Failed of string | Skipped of string

type result = {statuses: root_status String_map.t; order: string list}

exception Build_error of string

exception Deps_cycle of string list

let find_opt key kvs =
  List.find_opt (fun (k, _) -> k = key) kvs |> Option.map snd

let parse_deps s =
  String.split_on_char ',' s |> List.map String.trim
  |> List.filter (fun x -> x <> "")

let root_of_chunk (name : string) (chunk : Symbol_table.chunk_def) =
  let root_defs =
    List.filter
      (fun (d : Symbol_table.definition) -> d.is_root)
      chunk.definitions
  in
  if root_defs = [] then None
  else
    let build_cmd =
      List.fold_left
        (fun acc (d : Symbol_table.definition) ->
          match find_opt "build" d.options with Some x -> Some x | None -> acc )
        None root_defs
    in
    let run_cmd =
      List.fold_left
        (fun acc (d : Symbol_table.definition) ->
          match find_opt "run" d.options with Some x -> Some x | None -> acc )
        None root_defs
    in
    let deps =
      List.fold_left
        (fun acc (d : Symbol_table.definition) ->
          match find_opt "deps" d.options with
          | Some x ->
              acc @ parse_deps x
          | None ->
              acc )
        [] root_defs
      |> List.sort_uniq String.compare
    in
    Some {name; build_cmd; run_cmd; deps}

let roots_of_symbols (symbols : Symbol_table.build_result) =
  let chunks = Symbol_table.chunks symbols in
  Symbol_table.String_map.fold
    (fun name chunk acc ->
      match root_of_chunk name chunk with Some r -> r :: acc | None -> acc )
    chunks []
  |> List.rev

let validate_deps roots =
  let names =
    List.fold_left (fun s r -> String_set.add r.name s) String_set.empty roots
  in
  List.iter
    (fun r ->
      List.iter
        (fun dep ->
          if not (String_set.mem dep names) then
            raise
              (Build_error
                 ("root '" ^ r.name ^ "' depends on unknown root '" ^ dep ^ "'")
              ) )
        r.deps )
    roots

let topo_sort_roots roots =
  let by_name =
    List.fold_left (fun m r -> String_map.add r.name r m) String_map.empty roots
  in
  let state = Hashtbl.create 32 in
  let stack = ref [] in
  let order = ref [] in
  let rec cycle_from node = function
    | [] ->
        [node]
    | x :: xs ->
        if x = node then node :: x :: xs else cycle_from node xs
  in
  let rec visit name =
    match Hashtbl.find_opt state name with
    | Some 2 ->
        ()
    | Some 1 ->
        raise (Deps_cycle (cycle_from name !stack))
    | _ ->
        ( Hashtbl.replace state name 1 ;
          stack := name :: !stack ;
          match String_map.find_opt name by_name with
          | None ->
              ()
          | Some root ->
              List.iter visit root.deps ) ;
        begin match !stack with _ :: tl -> stack := tl | [] -> ()
        end ;
        Hashtbl.replace state name 2 ;
        order := name :: !order
  in
  List.iter (fun r -> visit r.name) roots ;
  List.rev !order

let is_ok = function
  | Succeeded | Cached ->
      true
  | Failed _ | Skipped _ ->
      false

let run_cmd ~dry_run cmd =
  if dry_run then (
    print_endline ("would run: " ^ cmd) ;
    Succeeded )
  else
    match Unix.system cmd with
    | Unix.WEXITED 0 ->
        Succeeded
    | Unix.WEXITED n ->
        Failed (Printf.sprintf "command failed (exit %d): %s" n cmd)
    | Unix.WSIGNALED n ->
        Failed (Printf.sprintf "command signaled (%d): %s" n cmd)
    | Unix.WSTOPPED n ->
        Failed (Printf.sprintf "command stopped (%d): %s" n cmd)

let run ?(dry_run = false) ?only (symbols : Symbol_table.build_result) =
  let roots = roots_of_symbols symbols in
  validate_deps roots ;
  let order = topo_sort_roots roots in
  let by_name =
    List.fold_left (fun m r -> String_map.add r.name r m) String_map.empty roots
  in
  let statuses = ref String_map.empty in
  let active =
    match only with
    | None ->
        None
    | Some names ->
        Some
          (List.fold_left
             (fun s n -> String_set.add n s)
             String_set.empty names )
  in
  let get_status n =
    match String_map.find_opt n !statuses with
    | Some s ->
        s
    | None ->
        Skipped "not processed"
  in
  List.iter
    (fun name ->
      match String_map.find_opt name by_name with
      | None ->
          ()
      | Some root ->
          let is_active =
            match active with None -> true | Some s -> String_set.mem name s
          in
          if not is_active then statuses := String_map.add name Cached !statuses
          else
            let blocked_dep =
              List.find_opt (fun d -> not (is_ok (get_status d))) root.deps
            in
            let status =
              match blocked_dep with
              | Some dep ->
                  Skipped ("dependency failed: " ^ dep)
              | None -> (
                match root.build_cmd with
                | Some cmd -> (
                  match run_cmd ~dry_run cmd with
                  | Succeeded | Cached -> (
                    match root.run_cmd with
                    | Some cmd2 ->
                        run_cmd ~dry_run cmd2
                    | None ->
                        Succeeded )
                  | Failed _ as f ->
                      f
                  | Skipped _ as s ->
                      s )
                | None -> (
                  match root.run_cmd with
                  | Some cmd ->
                      run_cmd ~dry_run cmd
                  | None ->
                      Succeeded ) )
            in
            statuses := String_map.add name status !statuses )
    order ;
  {statuses= !statuses; order}

let has_failures (r : result) =
  String_map.exists
    (fun _ -> function
      | Failed _ -> true | Succeeded | Cached | Skipped _ -> false )
    r.statuses

let print_result (r : result) =
  List.iter
    (fun name ->
      match String_map.find_opt name r.statuses with
      | None ->
          ()
      | Some Succeeded ->
          print_endline ("build ok: " ^ name)
      | Some Cached ->
          print_endline ("build cached: " ^ name)
      | Some (Failed msg) ->
          prerr_endline ("build failed: " ^ name ^ " - " ^ msg)
      | Some (Skipped msg) ->
          prerr_endline ("build skipped: " ^ name ^ " - " ^ msg) )
    r.order
