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
 (description
  "Summer is a set of libraries and tools for developing full stack web applications in OCaml")
 (tags
  ("http" "form" "web"))
 (depends
  (ocaml
   (>= 4.10.0))
  (fmt
   (>= 0.8.9))
  (angstrom
   (>= 0.15.0))
  lwt
  (bigstringaf
   (>= 0.7.0))
  (http-multipart-formdata
   (>= 2.0.1))
  (ppx_expect :with-test)
  (ppx_deriving :with-test)))
