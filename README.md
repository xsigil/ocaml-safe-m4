# ocaml-safe-m4

A robust, type-safe, and sandboxed POSIX-compliant M4 macro processor implemented in OCaml.

`ocaml-safe-m4` provides deterministic, hermetic macro evaluation designed for CLI tooling, secure template preprocessing, and modern code generation pipelines. It eliminates dangerous side effects (such as arbitrary command execution via `syscmd` and path traversals via `include`) while supplying modern raw-escaping primitives (`h`, `html_escape`, `json_escape`, `sh_quote`).

---

## Key Features

- **POSIX-Compliant Core**:
  - Full support for `define`, `undefine`, `ifdef`, `ifelse`, `dnl`, `shift`, and `defn`.
  - Nested quotation tracking (`` `...` ``) and custom delimiters via `changequote` and `changecom`.
  - Multi-channel diversion buffering via `divert` and `undivert`.
- **String & Arithmetic Engine**:
  - Standard manipulation primitives: `len`, `index`, `substr`, `translit` (with range expansion `a-z`), `incr`, and `decr`.
  - Comprehensive `eval`: arithmetic, relational/equality, bitwise (`&`, `|`, `^`, `~`, `<<`, `>>`), logical (`&&`, `||`), parenthesized nesting, and radix outputs (binary, octal, hex).
- **Hermetic Sandboxing & Security**:
  - `syscmd` / `esyscmd` omitted/disabled by default against arbitrary command execution (RCE).
  - Path-traversal protection on `include` / `sinclude` restricted to a specified sandbox root (`--sandbox`).
  - Built-in contextual escaping (`h` / `html_escape`, `json_escape`, `sh_quote`) routed directly without rescanning vulnerabilities.
- **Fast CLI & Zero Dependencies**:
  - Native standard input/file streaming with POSIX `-DNAME=VALUE` and `-DNAME` command-line macro definitions.

---

## Project Structure

```text
ocaml-safe-m4/
├── bin/
│   └── main.ml        # CLI driver (-D flag parsing & stream pipeline)
├── lib/
│   ├── builtins.ml    # POSIX built-in functions & eval AST
│   ├── engine.ml      # Recursive-descent macro evaluation engine
│   ├── lexer.ml       # Stack-based tokenization & quote/comment scanner
│   └── safe_m4.mli    # Public library interface
└── test/
    └── test_posix.ml  # POSIX compliance & regression test suite

```

---

## Build & Test

```bash
# Build library and CLI executable
dune build bin/main.exe

# Run test suite
dune runtest

```

---

## CLI Usage

```bash
# Macro expansion via pipeline
printf 'GREET(`Safe M4\x27) | eval(COUNT * 2) | sh_quote(COMMAND)\n' \
  | ./_build/default/bin/main.exe -DGREET='Hello, $1!' -DCOUNT=21 -DCOMMAND='rm -rf /'

# Output:
# Hello, Safe M4! | 42 | 'rm -rf /'

```

---

## License

MIT
