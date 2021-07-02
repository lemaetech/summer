(lang dune 2.9)

(name summer)

(generate_opam_files true)

(version 0.0.1)

(source
 (github lemaetech/summer))

(license MPL-2.0)

(authors "Bikal Lem, <gbikal@gmail.com>")

(maintainers "Bikal Lem")

(package
 (name summer)
 (synopsis "OCaml Summer libraries and tools")
 (tags
  ("http" "form" "web"))
 (depends
  (ocaml
   (>= 4.10.0))
  (fmt
   (>= 0.8.9))
  lwt
  (reparse
   (>= 3.0.0))
  (reparse-lwt
   (>= 3.0.0))
  (reparse-lwt-unix
   (>= 3.0.0))
  (ppx_expect :with-test)
  (ppx_deriving :with-test)))
