(lang dune 3.22)

(name chat)

(generate_opam_files true)

(source
 (codeberg rtshkmr/chat))

(authors "Ritesh Kumar <hello@rtshkmr.com>")

(maintainers "Ritesh <hello@rtshkmr.com>")

(license GPL-3.0-only)

(documentation https://url/to/documentation)

(package
 (name chat)
 (synopsis
  "A simple 1:1 chat server/client application for a takehome assessment")
 (description
  "This is my implementation for the takehome assessment given by Ahrefs Pte. Ltd. for the OCaml Developer role")
 (depends
  ocaml
  (alcotest :with-test)
  (ocaml-lsp-server :with-dev)
  (merlin :with-dev)
  (ocamlformat :with-dev)
  (utop :with-dev))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
