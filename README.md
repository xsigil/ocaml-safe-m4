# ocaml-safe-m4

A formal, type-safe, and sandboxed reference implementation of the POSIX M4 macro processor written in OCaml.

`ocaml-safe-m4` brings deterministic, hermetic macro evaluation to the OCaml ecosystem. Built with `ocamllex` and `menhir`, it eliminates dangerous side effects (such as arbitrary command execution via `syscmd` and path traversals via `include`) while providing first-class escaping primitives (e.g., Rails-style `h()` HTML escaping) for modern web stacks (Dream, Caqti, Lwt) and structured code generators.

---

## Key Features

- **Formal & Type-Safe AST**:
  - Macro expansion and token streams expressed via algebraic data types (ADTs) and pattern matching.
  - Deterministic evaluation semantics with strict recursion limits.
- **Hermetic Sandbox by Default**:
  - `syscmd` / `esyscmd` disabled by default.
  - Sandboxed `include` enforcing directory isolation against path traversal attacks.
- **Modern Escaping Primitives**:
  - `h(...)` / `html_escape(...)`: HTML entity escaping for XSS prevention.
  - `json_escape(...)`: Safe embedding into JSON string literals.
  - `sh_quote(...)`: Single-quote sanitization for shell contexts.
- **Seamless Modern OCaml Integration**:
  - Ready to plug into Dream web applications or CLI pipelines as a lightweight, zero-bloat template engine.

---

## Project Structure


```

ocaml-safe-m4/
├── bin/          # CLI executable
├── lib/
│   ├── ast.ml    # Macro syntax tree & data types
│   ├── lexer.mll # Lexer specification (ocamllex)
│   ├── parser.mly# Grammar specification (menhir)
│   ├── eval.ml   # Pure expansion engine & diversions
│   └── escape.ml # HTML/JSON escaping functions
└── test/         # Unit & regression tests

```

---

## Build & Test

```bash
dune build
dune runtest

```

---

## License

MIT
