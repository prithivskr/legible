# Literate Programming System — Language Specification & Feature Document

**Project Name: `legible`
**Implementation Language:** OCaml
**Lexer/Parser:** Hand-rolled
**File Extension:** `.lit`

---

## Table of Contents

1. [Document Model](#1-document-model)
2. [Syntax Reference](#2-syntax-reference)
3. [Chunk System](#3-chunk-system)
4. [Constraint & Annotation Language](#4-constraint--annotation-language)
5. [Build Metadata](#5-build-metadata)
6. [Tangle Semantics](#6-tangle-semantics)
7. [Dependency Resolution & DAG](#7-dependency-resolution--dag)
8. [Incremental Compilation](#8-incremental-compilation)
9. [Weave / HTML Output](#9-weave--html-output)
10. [CLI Interface](#10-cli-interface)
11. [Error Model](#11-error-model)
12. [OCaml Module Structure](#12-ocaml-module-structure)

---

## 1. Document Model

A `.lit` file is a sequence of **prose blocks** and **chunk blocks**, read top-to-bottom. Prose is arbitrary text (treated as Markdown). Chunks are named, typed code fragments that can reference each other and be tangled into source files.

### Conceptual structure

```
Document
  ├── ProseBlock (raw Markdown text)
  ├── ChunkDef   (named code fragment with metadata)
  ├── ProseBlock
  └── ChunkDef
```

A chunk is either:
- A **fragment chunk** — a reusable named piece, not directly emitted to a file.
- A **root chunk** — marked as a file output target; anchors the tangle process.

Root chunks are the entry points of the DAG. Every fragment chunk must be reachable from at least one root chunk (this is an optional lint warning, not a hard error).

---

## 2. Syntax Reference

### 2.1 Chunk Definition

```
@chunk{chunk-name}[options]
<body>
@end
```

- `chunk-name` is any string not containing `{`, `}`, or newline. Whitespace is trimmed.
- `[options]` is optional. See §3 for option grammar.
- The body is everything between the header line and the `@end` line, preserving all whitespace and indentation exactly.
- `@end` must appear on its own line (leading/trailing whitespace is ignored).

**Example:**

```
@chunk{parse-header}[lang=ocaml]
let parse_header input =
  match input with
  | [] -> Error "empty"
  | x :: _ -> Ok x
@end
```

### 2.2 Root Chunk (File Output)

```
@root{output-filename}[options]
<body>
@end
```

A root chunk's name IS the output file path (relative to the project root, or as configured). Root chunks may reference fragment chunks. They can also carry build metadata options (see §5).

**Example:**

```
@root{src/parser.ml}[lang=ocaml, build=dune build, run=dune exec ./main.exe]
@{parse-header}
@{parse-body}
@{parse-footer}
@end
```

### 2.3 Chunk Reference

Inside any chunk body, reference another chunk with:

```
@{chunk-name}
```

A reference may appear inline within a line. The reference is replaced by the full (recursively expanded) body of the named chunk, with indentation matching the column position of the `@{` sigil.

**Example:**

```
@chunk{main-function}[lang=ocaml]
let () =
  @{setup-code}
  @{run-loop}
@end
```

### 2.4 Additive Chunks

A chunk name can be defined more than once. Each definition **appends** to the chunk body in document order.

```
@chunk{helpers}[lang=ocaml]
let foo x = x + 1
@end

(* ... prose ... *)

@chunk{helpers}[lang=ocaml]
let bar x = x * 2
@end
```

When `@{helpers}` is expanded, both definitions are concatenated in order.

### 2.5 Annotations / Constraints

Annotations apply to a chunk or to the whole document:

```
@annotation{annotation-name}[args]
```

Annotations are placed on the line immediately before a `@chunk` or `@root` block, or at the top of the document for document-level constraints.

Multiple annotations may stack:

```
@annotation{once}
@annotation{lang-check}
@chunk{my-chunk}[lang=ocaml]
...
@end
```

See §4 for the full annotation catalog.

### 2.6 Prose Blocks

Any line that does not begin with `@chunk`, `@root`, `@end`, `@annotation`, or `@{...}` at the start of a line (outside a chunk body) is prose. Prose is collected verbatim and rendered as Markdown in the HTML output.

### 2.7 Comments

Line comments inside a chunk body are passed through unchanged to the tangled output (they are just text to `lit`). Document-level comments use:

```
@-- this is a lit comment, stripped from all output
```

---

## 3. Chunk System

### 3.1 Option Grammar

Options appear in `[...]` after a chunk or root header:

```
[key=value, key=value, ...]
```

Values are unquoted strings, trimmed of whitespace. A value may contain spaces but not commas or `]`.

**Recognized option keys:**

| Key      | Applies to     | Description                                              |
|----------|----------------|----------------------------------------------------------|
| `lang`   | chunk, root    | Language tag for syntax highlighting and type checks.   |
| `file`   | root only      | Alias for the chunk name; both specify output path.     |
| `build`  | root only      | Shell command to build this file's target.               |
| `run`    | root only      | Shell command to run after a successful build.           |
| `deps`   | root only      | Comma-separated list of other root filenames that must be tangled and built first. |
| `once`   | chunk          | Shorthand for `@annotation{once}`. |

Any unrecognized key is a warning, not an error.

### 3.2 Symbol Table

The symbol table maps chunk names to their list of body fragments (since additive chunks accumulate). It is built in a single top-to-bottom pass after parsing.

```
type fragment =
  | Text  of string
  | Ref   of string * int  (* name, column offset for indentation *)

type chunk_def = {
  name     : string;
  lang     : string option;
  is_root  : bool;
  out_file : string option;
  build    : string option;
  run      : string option;
  deps     : string list;
  bodies   : fragment list list;  (* one per definition, in order *)
  annotations : annotation list;
  loc      : location;
}
```

### 3.3 Scoping Rules

Chunk names are global and flat. There is no nesting or shadowing. If a name is undefined at tangle time, that is a fatal error with a source location.

---

## 4. Constraint & Annotation Language

Annotations are checked **before** tangling, in a dedicated semantic analysis pass. Failed constraints abort tangle unless `--warn-only` is passed.

### 4.1 Built-in Annotations

#### `@annotation{once}`

This chunk name must appear in exactly one definition (no additive extensions). Violation: error.

```
@annotation{once}
@chunk{config}[lang=ocaml]
let config = { debug = false }
@end
```

#### `@annotation{lang-check}`

Enforce that every chunk referenced from this chunk shares the same `lang` tag. Mixed-language references are normally allowed; this opts into strict checking.

```
@annotation{lang-check}
@chunk{typed-module}[lang=ocaml]
@{module-sig}
@{module-impl}
@end
```

#### `@annotation{require lang=<value>}`

Assert that this chunk has a specific lang tag. Error if the `lang` option is missing or different.

```
@annotation{require lang=ocaml}
@chunk{runtime}
(* this chunk has no lang declared — will error *)
@end
```

#### `@annotation{platform=<value>}`

Valid values: `posix`, `windows`, `any` (default). Marks a chunk as platform-specific. At tangle time, if the host platform doesn't match, the chunk body is replaced with a platform-appropriate stub (empty, or an `#error` directive for C, etc.) and a warning is emitted. Future extension: `--platform` flag overrides host detection.

```
@annotation{platform=posix}
@chunk{signal-handler}[lang=c]
signal(SIGPIPE, SIG_IGN);
@end
```

#### `@annotation{exclude-from lang=<value>}`

This chunk must never be referenced (directly or transitively) from a root chunk with the given lang. Acts as a "foreign reference fence."

```
@annotation{exclude-from lang=python}
@chunk{unsafe-c-block}[lang=c]
free(ptr);
@end
```

#### `@annotation{max-refs=<n>}`

This chunk may be referenced at most `n` times across the entire document. Useful for ensuring a critical initialization block isn't accidentally included multiple times via different paths. At tangle time, the reference count is computed over the *expanded* call graph, not just syntactic references.

```
@annotation{max-refs=1}
@chunk{global-init}[lang=c]
initialize_runtime();
@end
```

#### `@annotation{deprecated msg=<message>}`

Any reference to this chunk emits a warning with the given message. Used when migrating chunk names.

```
@annotation{deprecated msg=use parse-header-v2 instead}
@chunk{parse-header}[lang=ocaml]
...
@end
```

#### `@annotation{abstract}`

This chunk name is declared but has no body here; it must be defined in an additive extension before tangle. Useful for defining interface points in a literate document that a later section fills in.

```
@annotation{abstract}
@chunk{platform-hooks}[lang=ocaml]
@end
```

If tangle is invoked and no additive definitions of `platform-hooks` have been provided, error.

### 4.2 Document-Level Annotations

Placed before the first chunk or prose block:

```
@annotation{strict-lang}
```

Enables `lang-check` globally on all chunks. Equivalent to annotating every chunk with `@annotation{lang-check}`.

```
@annotation{no-additive}
```

Disables additive chunks globally. Every chunk name may only be defined once. Equivalent to `@annotation{once}` on every chunk.

### 4.3 Annotation Processing Pipeline

```
Parse → Build Symbol Table → Check Annotations → Build DAG → Cycle Detection → Tangle
```

Annotation errors are collected (not fail-fast) and reported together at the end of the annotation pass.

---

## 5. Build Metadata

Each root chunk may carry `build` and `run` options. After tangling all files, `lit` executes builds in dependency order.

### 5.1 Execution Order

1. Tangle all root chunks to their files.
2. Build the root chunk dependency graph using `deps` options.
3. Topologically sort the build graph.
4. For each root chunk in order: run `build`, then on success run `run`.

If a `build` command exits non-zero, dependent roots are skipped. Error output is forwarded to stderr with a prefix indicating which file's build failed.

### 5.2 `deps` Semantics

`deps` is a list of output filenames that must be tangled and built before this root:

```
@root{src/main.ml}[lang=ocaml, deps=src/lib.ml src/util.ml, build=dune build, run=./_build/default/src/main.exe]
```

`deps` values are space-separated (not comma, to distinguish from the option-level comma separator). They must be names of other root chunks in the same document.

Circular `deps` are detected separately from chunk reference cycles — a root chunk can depend on another root without referencing its chunks.

### 5.3 Environment Variables

All `build` and `run` commands are run with the following env vars set:

| Variable         | Value                             |
|------------------|-----------------------------------|
| `LIT_ROOT`       | Absolute path to the `.lit` file  |
| `LIT_OUT_FILE`   | This root chunk's output file     |
| `LIT_BUILD_DIR`  | Output directory (configurable)   |

---

## 6. Tangle Semantics

### 6.1 Expansion Algorithm

For each root chunk, perform recursive chunk expansion:

```
expand(fragment_list, indent_offset):
  for each fragment in fragment_list:
    Text s  -> emit (s with current indent applied to non-first lines)
    Ref(name, col) ->
      look up name in symbol table
      for each body in chunk_def.bodies (additive, in order):
        expand(body, col)
```

`col` is the 0-indexed column of the `@{` sigil. Every line of the expanded body (after the first) is re-indented to match col. The first line is inserted inline.

### 6.2 Indentation Handling

Indentation injection is **additive**: if the referenced chunk's body already has its own indentation, and the reference site has an offset of 4, then 4 spaces are prepended to each line. This matches the semantics of noweb and cweb and avoids surprising stripping.

Tabs are treated as single characters for column counting. A warning is emitted if mixed tabs and spaces are detected in indentation-sensitive positions.

### 6.3 Cycle Guard

A runtime cycle guard is maintained during expansion (a call stack of chunk names). If a chunk is encountered that is already on the stack, expansion halts with an error reporting the full cycle path. This should not be reachable if DAG validation passes, but is a safety net for implementation bugs.

### 6.4 Output

Each root chunk's expanded content is written to its output file path, relative to the directory containing the `.lit` file, unless an absolute path is given.

If the output file already exists, it is overwritten unless `--dry-run` is passed.

---

## 7. Dependency Resolution & DAG

### 7.1 Graph Construction

After building the symbol table, construct a directed graph:

- **Nodes:** every chunk name in the symbol table.
- **Edges:** `A → B` if chunk A's body contains a reference `@{B}`.
- Additive definitions share the same node; their edges are unioned.

### 7.2 Cycle Detection

Run DFS with three-color marking (white = unvisited, gray = in-progress, black = complete) over all nodes.

If a gray node is encountered during DFS from a gray node, a cycle exists. Collect the cycle path from the DFS stack and report it as a diagnostic with source locations for each reference in the cycle.

Cycle detection is run before tangle. A document with cycles is rejected entirely.

### 7.3 Topological Order

After cycle detection, produce a topological ordering of all chunks reachable from root chunks. Expansion proceeds in this order. (In practice, recursive expansion naturally follows topological order if implemented correctly, but the explicit ordering is useful for incremental change propagation.)

### 7.4 Reachability Analysis

After topological sort, compute the set of chunks reachable from all root chunks. Any chunk not in this set is unreachable. Emit a warning (not error) for each unreachable chunk:

```
warning: chunk 'old-helper' is defined but never referenced
  --> myfile.lit:42
```

---

## 8. Incremental Compilation

### 8.1 State File

`lit` maintains a hidden state file named `.lit-cache` in the same directory as the `.lit` file. It is a serialized record of the last successful build's state.

```
type cache = {
  lit_hash      : string;                  (* SHA-256 of the .lit source *)
  chunk_hashes  : (string * string) list;  (* chunk name -> SHA-256 of expanded body *)
  out_hashes    : (string * string) list;  (* output file path -> SHA-256 of file on disk *)
  build_success : (string * bool) list;    (* output file -> did last build succeed *)
}
```

### 8.2 Change Detection

On each invocation:

1. Hash the `.lit` source. If it matches `lit_hash`, the document hasn't changed — skip re-parsing and re-building the DAG.
2. Re-parse and rebuild the symbol table if the source changed.
3. For each root chunk, hash its **fully expanded** output (the string that would be written to disk). Compare to `chunk_hashes`.
4. If the expanded hash is unchanged, skip writing the file to disk and skip the `build` command for that root chunk. (File on disk is assumed valid if `out_hashes` matches.)
5. If the file on disk has been externally modified (disk hash ≠ `out_hashes`), force retangle and rebuild for that file regardless.

### 8.3 Dependency-Aware Invalidation

When any chunk's content changes, all root chunks that (transitively) reference that chunk are invalidated. The DAG is used to propagate invalidation upward:

```
invalidated = {changed_chunks}
repeat:
  for each root chunk r:
    if any chunk in the transitive closure of r's refs is in invalidated:
      add r to invalidated
until no new additions
```

Only invalidated roots are retangled and rebuilt.

### 8.4 Cache Invalidation

The cache is fully invalidated (and all roots rebuilt) if:
- The `.lit` source hash changes in a way that alters the chunk graph structure (new chunks, removed chunks, new edges).
- The `build` or `run` commands for a root change.
- `--force` is passed on the CLI.

### 8.5 Persistence Format

The `.lit-cache` file is a simple line-oriented text format (not binary) for debuggability:

```
VERSION 1
LIT_HASH <sha256>
CHUNK_HASH <name> <sha256>
CHUNK_HASH <name> <sha256>
OUT_HASH <path> <sha256>
BUILD_OK <path> true
BUILD_OK <path> false
```

---

## 9. Weave / HTML Output

### 9.1 Overview

`lit weave myfile.lit -o myfile.html` produces a self-contained HTML document with:

- Rendered prose (Markdown → HTML, using a minimal built-in renderer or a library)
- Syntax-highlighted code chunks (via highlight.js, loaded from CDN in the default template)
- Cross-reference tables for each chunk
- A navigation sidebar with all chunk names
- Build metadata displayed as a badge on root chunks

### 9.2 Chunk Rendering

Each chunk is rendered as:

```html
<div class="chunk" id="chunk-parse-header">
  <div class="chunk-header">
    <span class="chunk-name">⟨parse-header⟩</span>
    <span class="chunk-lang">ocaml</span>
    <span class="chunk-xref">Used in: <a href="#chunk-root-src-parser-ml">src/parser.ml</a></span>
  </div>
  <pre><code class="language-ocaml">
    ... body ...
  </code></pre>
  <div class="chunk-footer">
    Defined: <a href="...">here</a> (+2 additive parts)
  </div>
</div>
```

### 9.3 Cross-Reference Data

During the weave pass, build a reverse reference map: for each chunk, which other chunks reference it. This is used to generate "used in" links.

Also generate "see definition" links: for additive chunks, each definition site links to the others.

### 9.4 Root Chunk Badge

Root chunks display a metadata badge:

```
[FILE: src/parser.ml] [LANG: ocaml] [BUILD: dune build] [RUN: dune exec ...]
```

### 9.5 Annotations in HTML

Annotations are rendered as inline callout boxes above their chunk:

```
⚠ once — this chunk may only be defined once
🔒 exclude-from lang=python
```

### 9.6 Self-Contained Output

The default HTML output inlines a small amount of CSS and loads highlight.js from `https://cdnjs.cloudflare.com`. An `--offline` flag instead inlines a minified version of highlight.js for air-gapped use.

---

## 10. CLI Interface

```
lit <command> [flags] <file.lit>
```

### Commands

| Command        | Description                                                         |
|----------------|---------------------------------------------------------------------|
| `tangle`       | Expand chunks and write output files. Default command.              |
| `build`        | Tangle, then execute build/run commands.                            |
| `weave`        | Emit HTML output.                                                   |
| `check`        | Parse, validate annotations, detect cycles. No output files.        |
| `status`       | Show which roots are stale vs. up-to-date per the cache.            |
| `graph`        | Print the chunk dependency graph in DOT format (for Graphviz).      |
| `clean`        | Delete all tangled output files and the `.lit-cache`.               |

### Flags

| Flag              | Description                                               |
|-------------------|-----------------------------------------------------------|
| `-o <path>`       | Output path (for `weave`) or output directory (for `tangle`) |
| `--force`         | Ignore cache, retangle and rebuild everything.            |
| `--dry-run`       | Show what would be written/run without doing it.          |
| `--warn-only`     | Treat annotation violations as warnings, not errors.      |
| `--platform <p>`  | Override host platform for `@annotation{platform=...}`.   |
| `--offline`       | Inline assets in weave output (no CDN).                   |
| `--no-cache`      | Don't read or write the `.lit-cache` file.                |
| `--color`         | Force ANSI color in diagnostics (auto-detected by default)|

---

## 11. Error Model

### 11.1 Phases and Error Categories

| Phase              | Error type                              | Fatal? |
|--------------------|-----------------------------------------|--------|
| Lexing             | Unterminated chunk, invalid sigil       | Yes    |
| Parsing            | Missing `@end`, malformed options       | Yes    |
| Symbol Table       | (none — all names allowed)              | —      |
| Annotation Check   | Constraint violation                    | Collected, then fatal |
| DAG Construction   | Undefined reference                     | Fatal  |
| Cycle Detection    | Cycle in chunk graph                    | Fatal  |
| Build Dep Check    | Cycle in root dep graph                 | Fatal  |
| Tangle             | (internal only — guarded by above)      | Fatal  |
| Build Execution    | Non-zero exit from build/run            | Per-root |

### 11.2 Diagnostic Format

All diagnostics use a consistent format:

```
error[E003]: chunk 'parse-body' references undefined chunk 'tokenize'
  --> myfile.lit:87:12
   |
87 |   @{tokenize}
   |   ^^^^^^^^^^^ undefined reference
   |
   = help: did you mean 'tokenizer'?
```

Error codes are `E###` (errors) and `W###` (warnings). A table of codes is maintained in the implementation.

### 11.3 Error Collection

All passes collect errors into a diagnostic accumulator. After each pass, if any errors are present, they are printed together and the tool exits. This means you see all errors from the annotation pass at once, not just the first.

The lexer and parser are the only fail-fast passes, since subsequent passes require a valid AST.

---

## 12. OCaml Module Structure

```
lit/
  bin/
    main.ml          -- CLI entry point, command dispatch
  lib/
    lit.ml           -- Top-level library entry, re-exports
    source.ml        -- Source file + location tracking
    lexer.ml         -- Hand-rolled lexer, returns token stream
    parser.ml        -- Hand-rolled recursive descent parser, produces AST
    ast.ml           -- AST type definitions
    symbol_table.ml  -- Build symbol table from AST
    annotations.ml   -- Annotation checking pass
    dag.ml           -- Graph construction, cycle detection, topo sort
    tangle.ml        -- Chunk expansion, indentation injection, file output
    build.ml         -- Build command execution, dependency ordering
    weave.ml         -- HTML emission
    cache.ml         -- .lit-cache read/write, hash computation, invalidation
    diagnostics.ml   -- Error/warning accumulator, formatting, printing
  test/
    test_lexer.ml
    test_parser.ml
    test_dag.ml
    test_tangle.ml
    test_annotations.ml
    test_incremental.ml
  dune
  dune-project
  lit.opam
```

### Key Interface Signatures (sketch)

```ocaml
(* diagnostics.ml *)
type severity = Error | Warning
type diagnostic = { code: string; msg: string; loc: Source.loc; notes: string list }
type accumulator
val create   : unit -> accumulator
val add      : accumulator -> severity -> diagnostic -> unit
val has_errors : accumulator -> bool
val print_all  : accumulator -> unit

(* dag.ml *)
type graph
val build           : Ast.chunk_def list -> graph
val detect_cycles   : graph -> (string list) option  (* None = acyclic *)
val topo_sort       : graph -> string list
val reachable_from  : graph -> string list -> string list  (* roots -> reachable names *)
val to_dot          : graph -> string

(* tangle.ml *)
val expand_chunk : Symbol_table.t -> string -> string  (* chunk name -> expanded text *)
val tangle_all   : Symbol_table.t -> dag.graph -> unit (* writes output files *)

(* cache.ml *)
type cache
val load         : string -> cache option             (* path to .lit-cache *)
val save         : string -> cache -> unit
val stale_roots  : cache -> Symbol_table.t -> string list  (* which roots need rebuild *)
val update       : cache -> string -> string -> cache  (* update one root's hashes *)
```

---

## Appendix A: Complete Example

```
@-- A simple calculator in OCaml
@-- Demonstrates: root chunks, fragments, additive chunks, annotations, build metadata

# A Simple Calculator

This literate program implements a basic integer calculator.

@annotation{once}
@chunk{type-defs}[lang=ocaml]
type expr =
  | Num of int
  | Add of expr * expr
  | Mul of expr * expr
@end

## Evaluation

@chunk{eval}[lang=ocaml]
let rec eval = function
  | Num n      -> n
  | Add (a, b) -> eval a + eval b
  | Mul (a, b) -> eval a * eval b
@end

## Entry Point

@root{calc.ml}[lang=ocaml, build=ocamlfind ocamlopt -package str -linkpkg calc.ml -o calc, run=./calc]
@{type-defs}

@{eval}

let () =
  let result = eval (Add (Num 1, Mul (Num 2, Num 3))) in
  Printf.printf "Result: %d\n" result
@end

Running `lit build calc.lit` will tangle `calc.ml`, compile it with ocamlfind, and run it.
```

---

## Appendix B: Error Code Table

| Code  | Description                                      |
|-------|--------------------------------------------------|
| E001  | Unterminated chunk (missing `@end`)              |
| E002  | Malformed chunk header                           |
| E003  | Reference to undefined chunk                    |
| E004  | Cycle detected in chunk dependency graph         |
| E005  | Cycle detected in root build dependency graph    |
| E006  | `@annotation{once}` violated (duplicate def)     |
| E007  | `@annotation{abstract}` chunk never defined      |
| E008  | `@annotation{require lang=}` mismatch            |
| E009  | `@annotation{exclude-from}` violated             |
| E010  | `@annotation{max-refs}` exceeded                 |
| E011  | `@annotation{lang-check}` cross-language ref     |
| E012  | Root chunk `deps` refers to unknown root         |
| W001  | Unreachable chunk (defined but never referenced) |
| W002  | `@annotation{deprecated}` reference              |
| W003  | Unrecognized chunk option key                    |
| W004  | Mixed tab/space indentation                      |
| W005  | Platform mismatch for `@annotation{platform=}`   |
| W006  | Build command missing for root chunk             |
