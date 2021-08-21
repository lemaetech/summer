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
  (cstruct
   (>= 6.0.1))
  (tyxml
   (>= 4.5.0))
  (http-multipart-formdata
   (>= 3.1.0))
  (http-cookie
   (>= 4.2.0))
  (wtr
   (>= 2.0.0))
  (csexp
   (>= 1.5.1))
  (angstrom
   (>= 0.15.0))
  (uri
   (>= 4.2.0))
  (fmt
   (>= 0.8.9))
  (lwt
   (>= 5.4.1))
  (base64
   (>= 3.5.0))
  (mirage-crypto
   (>= 0.10.3))
  (mirage-crypto-rng
   (>= 0.10.3))
  (ppx_expect :with-test)
  (ppx_deriving :with-test)))
