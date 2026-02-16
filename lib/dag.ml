open Ast
module String_map = Map.Make (String)
module String_set = Set.Make (String)

type t = {adjacency: String_set.t String_map.t; roots: string list}

type analysis = {graph: t; topo: string list; unreachable: string list}

exception Cycle_error of string list

let refs_of_body body =
  List.fold_left
    (fun acc -> function Ref (name, _) -> name :: acc | Text _ -> acc)
    [] body

let refs_of_chunk (chunk : Symbol_table.chunk_def) =
  List.fold_left
    (fun acc body -> List.rev_append (refs_of_body body) acc)
    [] chunk.bodies

let defined_names table =
  Symbol_table.String_map.fold
    (fun name _ acc -> String_set.add name acc)
    table String_set.empty

let build_graph chunks =
  let names = defined_names chunks in
  let adjacency =
    Symbol_table.String_map.fold
      (fun name (chunk : Symbol_table.chunk_def) acc ->
        let deps =
          refs_of_chunk chunk
          |> List.fold_left
               (fun s dep ->
                 if String_set.mem dep names then String_set.add dep s else s )
               String_set.empty
        in
        String_map.add name deps acc )
      chunks String_map.empty
  in
  let roots =
    Symbol_table.String_map.fold
      (fun name (chunk : Symbol_table.chunk_def) acc ->
        let is_root =
          List.exists
            (fun (d : Symbol_table.definition) -> d.is_root)
            chunk.definitions
        in
        if is_root then name :: acc else acc )
      chunks []
    |> List.rev
  in
  {adjacency; roots}

let cycle_from_stack stack node =
  let rec drop = function
    | [] ->
        [node]
    | x :: xs ->
        if x = node then node :: x :: xs else drop xs
  in
  drop stack

let topo_sort (graph : t) =
  let state = Hashtbl.create 32 in
  let stack = ref [] in
  let order = ref [] in
  let rec visit n =
    match Hashtbl.find_opt state n with
    | Some 2 ->
        ()
    | Some 1 ->
        raise (Cycle_error (cycle_from_stack !stack n))
    | _ ->
        Hashtbl.replace state n 1 ;
        stack := n :: !stack ;
        let deps =
          match String_map.find_opt n graph.adjacency with
          | Some s ->
              s
          | None ->
              String_set.empty
        in
        String_set.iter visit deps ;
        begin match !stack with _ :: tl -> stack := tl | [] -> ()
        end ;
        Hashtbl.replace state n 2 ;
        order := n :: !order
  in
  String_map.iter (fun name _ -> visit name) graph.adjacency ;
  List.rev !order

let reachable_from_roots (graph : t) =
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
  List.iter dfs graph.roots ; seen

let analyze chunks =
  let graph = build_graph chunks in
  let topo = topo_sort graph in
  let reachable = reachable_from_roots graph in
  let unreachable =
    String_map.fold
      (fun name _ acc -> if Hashtbl.mem reachable name then acc else name :: acc)
      graph.adjacency []
    |> List.rev
  in
  {graph; topo; unreachable}
