(lang dune 3.19)

(name anthropic)

(generate_opam_files true)

(source
 (github tmattio/ocaml-anthropic))

(authors "Thibaut Mattio <thibaut.mattio@gmail.com>")

(maintainers "Thibaut Mattio <thibaut.mattio@gmail.com>")

(license ISC)

(implicit_transitive_deps false)

(package
 (name anthropic)
 (synopsis "OCaml bindings for the Anthropic API")
 (description
  "This library provides type-safe OCaml bindings to the Anthropic API,\n   enabling interaction with Claude models. It supports both synchronous and\n   streaming responses, tool use, batch processing, and file uploads (beta).")
 (depends
  ocaml
  cohttp
  cohttp-eio
  (eio
   (>= 1.2))
  (tls
   (>= 2.0.1))
  tls-eio
  ca-certs
  mirage-crypto-rng
  yojson
  ppx_deriving_yojson
  logs)
 (tags
  ("anthropic" "claude" "ai" "llm" "api" "client" "streaming" "eio")))
