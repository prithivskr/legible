type fragment = Text of string | Ref of string * int

type chunk_def =
  { name: string
  ; is_root: bool
  ; options: string * string list
  ; body: fragment list }

type annotation = {name: string; args: string * string list}

type top_level =
  | Prose of fragment list
  | ChunkDef of chunk_def
  | Annotation of annotation

type document = top_level list
