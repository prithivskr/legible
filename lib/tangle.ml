open Ast

type output = {path: string; content: string}

exception Tangle_error of string

let indent_multiline ~indent_first column text =
  if column <= 0 || String.length text = 0 then text
  else
    let spaces = String.make column ' ' in
    let buf = Buffer.create (String.length text + 16) in
    if indent_first then Buffer.add_string buf spaces ;
    let last = String.length text - 1 in
    for i = 0 to last do
      let c = text.[i] in
      Buffer.add_char buf c ;
      if c = '\n' && i < last then Buffer.add_string buf spaces
    done ;
    Buffer.contents buf

let ensure_parent_dirs path =
  let rec mkdir_p dir =
    if dir = "" || dir = "." || Sys.file_exists dir then ()
    else (
      mkdir_p (Filename.dirname dir) ;
      Unix.mkdir dir 0o755 )
  in
  mkdir_p (Filename.dirname path)

let write_output ?base_dir (out : output) =
  let path =
    match base_dir with
    | None ->
        out.path
    | Some dir ->
        Filename.concat dir out.path
  in
  ensure_parent_dirs path ;
  let oc = open_out_bin path in
  output_string oc out.content ;
  close_out oc

let write_outputs ?base_dir outs = List.iter (write_output ?base_dir) outs

let expand (chunks : Symbol_table.t) =
  let ends_with_newline s =
    let len = String.length s in
    len > 0 && s.[len - 1] = '\n'
  in
  let analysis = Dag.analyze chunks in
  let expanded = Hashtbl.create 64 in
  let drop_one_trailing_newline s =
    let len = String.length s in
    if len > 0 && s.[len - 1] = '\n' then String.sub s 0 (len - 1) else s
  in
  let get_chunk name =
    match Symbol_table.find name chunks with
    | Some c ->
        c
    | None ->
        raise (Tangle_error ("unknown chunk " ^ name))
  in
  let render_ref owner (name, column) =
    let dep =
      match Hashtbl.find_opt expanded name with
      | Some s ->
          s
      | None ->
          if Symbol_table.mem name chunks then
            raise
              (Tangle_error
                 ("internal expansion order error at " ^ owner ^ " to " ^ name)
              )
          else
            raise
              (Tangle_error
                 ("undefined chunk reference in " ^ owner ^ " to " ^ name) )
    in
    (dep, column)
  in
  let render_body owner body =
    let buf = Buffer.create 128 in
    let rec loop prev_ended_newline = function
      | [] ->
          ()
      | frag :: rest -> (
        match frag with
        | Text s ->
            Buffer.add_string buf s ;
            loop (ends_with_newline s) rest
        | Ref (name, column) ->
            let dep, column = render_ref owner (name, column) in
            let dep =
              indent_multiline ~indent_first:prev_ended_newline column dep
            in
            let dep =
              match rest with
              | Text "\n" :: _ ->
                  drop_one_trailing_newline dep
              | _ ->
                  dep
            in
            Buffer.add_string buf dep ;
            loop (ends_with_newline dep) rest )
    in
    loop true body ; Buffer.contents buf
  in
  let render_chunk (chunk : Symbol_table.chunk_def) =
    let buf = Buffer.create 256 in
    List.iter
      (fun (d : Symbol_table.definition) ->
        Buffer.add_string buf (render_body chunk.name d.body) )
      chunk.definitions ;
    Buffer.contents buf
  in
  List.iter
    (fun name ->
      let chunk = get_chunk name in
      Hashtbl.replace expanded name (render_chunk chunk) )
    analysis.topo ;
  analysis.graph.roots
  |> List.map (fun name ->
      let content =
        match Hashtbl.find_opt expanded name with
        | Some s ->
            s
        | None ->
            raise (Tangle_error ("missing expanded root chunk " ^ name))
      in
      {path= name; content} )
