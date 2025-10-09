(lang dune 3.20)

(name ocron)

(generate_opam_files true)

(source
 (github whereistejas/ocron))

(authors "Tejas Sanap <email@whereistejas.xyz>")

(maintainers "Tejas Sanap <email@whereistejas.xyz>")


(license "MIT")

(package
 (name ocron)
 (synopsis "BSD-compliant crontab parser")
 (depends
  ocaml
  base
  core
  core_unix
  sexplib
  ppx_sexp_conv
  ppx_compare
  ppx_deriving
  (ppx_inline_test :with-test)
  (ppx_assert :with-test)))
