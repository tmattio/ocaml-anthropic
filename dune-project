(lang dune 3.19)

(name anthropic)

(generate_opam_files true)

(source
 (github tmattio/ocaml-anthropic))

(authors "Thibaut Mattio <thibaut.mattio@gmail.com>")

(maintainers "Thibaut Mattio <thibaut.mattio@gmail.com>")

(license ISC)

(documentation https://tmattio.github.io/ocaml-anthropic/)

(package
 (name anthropic)
 (synopsis "OCaml bindings for the Anthropic API")
 (description
  "This library provides type-safe OCaml bindings to the Anthropic API,
   enabling interaction with Claude models. It supports both synchronous and
   streaming responses, tool use, batch processing, and file uploads (beta).")
 (depends
  ocaml cohttp cohttp-eio eio tls-eio ca-certs mirage-crypto-rng yojson ppx_deriving_yojson)
 (tags
  ("anthropic" "claude" "ai" "llm" "api" "client" "streaming" "eio")))
