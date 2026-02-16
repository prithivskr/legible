open Ast

module String_map = Map.Make (String)

type definition =
  { body: fragment list
  ; options: (string * string) list
  ; is_root: bool
  ; loc: loc
  ; annotations: annotation list }

type chunk_def =
  { name: string
  ; definitions: definition list
  ; bodies: fragment list list
  ; annotations: annotation list }

type t = chunk_def String_map.t

type build_result = {chunks: t; document_annotations: annotation list}

let empty = String_map.empty

let find name tbl = String_map.find_opt name tbl

let mem name tbl = String_map.mem name tbl

let chunks result = result.chunks

let document_annotations result = result.document_annotations

let add_chunk_def tbl (def : Ast.chunk_def) anns =
  let new_def =
    { body= def.body
    ; options= def.options
    ; is_root= def.is_root
    ; loc= def.loc
    ; annotations= anns }
  in
  match String_map.find_opt def.name tbl with
  | None ->
      String_map.add def.name
        { name= def.name
        ; definitions= [new_def]
        ; bodies= [def.body]
        ; annotations= anns }
        tbl
  | Some prev ->
      String_map.add def.name
        { prev with
          definitions= prev.definitions @ [new_def]
        ; bodies= prev.bodies @ [def.body]
        ; annotations= prev.annotations @ anns }
        tbl

let build (doc : document) =
  let pending = ref [] in
  let doc_annotations = ref [] in
  let tbl = ref empty in
  let flush_pending_to_doc () =
    if !pending <> [] then (
      doc_annotations := !doc_annotations @ List.rev !pending ;
      pending := [] )
  in
  List.iter
    (function
      | Annotation ann ->
          pending := ann :: !pending
      | ChunkDef def ->
          let anns = List.rev !pending in
          pending := [] ;
          tbl := add_chunk_def !tbl def anns
      | Prose _ ->
          flush_pending_to_doc ())
    doc ;
  flush_pending_to_doc () ;
  {chunks= !tbl; document_annotations= !doc_annotations}
