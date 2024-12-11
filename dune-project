(lang dune 3.17)

(name splitmix)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name splitmix)
 (synopsis "Native SplitMix implementation")
 (description "An implementation of the SplitMix algorithm using the C FFI of Ocaml")
 (depends ocaml dune)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
