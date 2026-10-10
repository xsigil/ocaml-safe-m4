(lang dune 3.0)
(name safe_m4)
(version 0.1.0)
(generate_opam_files true)

(package
 (name safe_m4)
 (synopsis "Secure POSIX-compliant M4 macro processor in OCaml")
 (description "Pure OCaml implementation of safe M4 macro expansion with sandboxing")
 (depends
  (ocaml (>= 4.14.0))
  dune))
