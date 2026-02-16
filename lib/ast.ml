type fragment = Text of string | Ref of string * int

type loc = Lexer.position

type chunk_def =
  { name: string
  ; is_root: bool
  ; options: (string * string) list
  ; body: fragment list
  ; loc: loc }

type annotation = {name: string; args: (string * string) list; loc: loc}

type top_level =
  | Prose of fragment list
  | ChunkDef of chunk_def
  | Annotation of annotation

type document = top_level list
