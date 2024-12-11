(lang dune 3.17)

(name unboxed_splitmix)

(generate_opam_files true)

(source
 (github alpha-convert/unboxed_splitmix))

(authors "Joseph Cutler <jwc@seas.upenn.edu>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name unboxed_splitmix)
 (synopsis "SplitMix implementation with Unboxed Types")
 (description "An implementation of the SplitMix algorithm using unboxed types")
 (depends ocaml__5_2_0+flambda2 dune)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
