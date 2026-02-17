# Legible

Literate programming toolchain.

## Install / Build

```sh
dune build ./bin/legible
```

Run via Dune:

```sh
legible <command> <file.lit>
```

## File Syntax (`.lit`)

### Chunk definition

```text
@chunk{chunk-name}[options]
<body>
@end
```

### Root chunk (output file)

Root chunk names are output paths:

```text
@root{path/to/output}[options]
<body>
@end
```

### Chunk reference

Inside a chunk body:

```text
@{other-chunk}
```

References expand inline. Indentation is injected based on the column of the `@{` sigil.

### Additive chunks

Defining the same chunk multiple times appends bodies in document order:

```text
@chunk{helpers}
...
@end

@chunk{helpers}
...
@end
```

### Annotations

```text
@annotation{name}[args]
```

Annotations apply to the next `@chunk`/`@root`. Document-level annotations appear before any chunk.

### Options

`[key=value, key=value, ...]` after a chunk/root header. Values are unquoted strings.

Recognized options:

- `lang` (chunk, root)
- `build` (root) shell command
- `run` (root) shell command
- `deps` (root) comma-separated root names (output paths)

## CLI

Commands:

- `tangle FILE.lit` expand roots and write output files
- `build FILE.lit` tangle then run root `build`/`run` commands (dependency-aware)
- `weave FILE.lit` render plain HTML (unstyled; class hooks only)
- `check FILE.lit` parse + validate + annotation checks + cycle detection
- `graph FILE.lit` print edges as `from -> to`
- `clean FILE.lit` delete root outputs and `.lit-cache`

Flags:

- `-o PATH` output override:
  - `weave`: output HTML path (default: `<file>.html`)
  - `tangle`: if multiple roots, `PATH` is a base directory; if one root, overrides the output path
  - `build`: same behavior for the tangle step
- `--dry-run` show writes/commands without executing
- `--warn-only` treat annotation errors as warnings
- `--platform posix|windows` override platform for platform checks
- `--force` ignore cache (retangle/rebuild everything)
- `--no-cache` do not read/write `.lit-cache`

## Weave (HTML)

`weave` turns a `.lit` file into unstyled HTML and only attaches class names/`data-*` attributes for styling.

Prose mapping:

- `# Title`..`###### Title` -> `<h1 class="prose-h1">`..`<h6 class="prose-h6">`
- `---` -> `<hr class="prose-hr"/>`
- any other non-empty line -> `<p class="prose-p">...`

Chunk/annotation structure (classes):

- `.legible-document` root container
- `.prose-block` prose section
- `.annotation-block` annotation section
- `.chunk-block.fragment` / `.chunk-block.root` chunk sections (`data-name`, `data-kind`)
- `.chunk-ref` reference marker (`data-target`)

## Annotations Reference

Chunk-level:

- `@annotation{once}`: chunk must have exactly one definition
- `@annotation{abstract}`: chunk must have at least one non-empty definition body
- `@annotation{require}[lang=<value>]`: this chunk must declare `lang=<value>`
- `@annotation{lang-check}`: all referenced chunks must have the same `lang`
- `@annotation{exclude-from}[lang=<value>]`: chunk must not be reachable from any root with that lang
- `@annotation{max-refs}[n=<int>]`: chunk may appear at most `n` times in the expanded call graph
- `@annotation{deprecated}[msg=<message>]`: referencing this chunk emits a warning
- `@annotation{platform}[value=posix|windows|any]`: platform mismatch emits a warning (tangle-time policy)

Document-level:

- `@annotation{strict-lang}`: applies `lang-check` to every chunk
- `@annotation{no-additive}`: applies `once` to every chunk

## Cache (`.lit-cache`)

When cache is enabled (default unless `--no-cache` or `-o` is used), `.lit-cache` is written next to the
`.lit` file and is used to skip:

- re-writing root outputs whose expanded content and on-disk hash match the cache
- re-running build/run commands for cached roots

`--force` bypasses the cache.

