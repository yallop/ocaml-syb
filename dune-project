(lang dune 3.21)

(name syb)

(generate_opam_files true)

(source
 (github yallop/ocaml-syb))

(authors "Jeremy Yallop <yallop@gmail.com>")
(maintainers "Jeremy Yallop <yallop@gmail.com>")

(license MIT)

(package
 (name syb)
 (synopsis "Scrap Your Boilerplate for OCaml")
 (depends ocaml higher ounit2))
