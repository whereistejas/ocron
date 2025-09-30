(lang dune 3.20)

(name ocron)

(generate_opam_files true)

(source
 (github whereistejas/ocron))

(authors "Tejas Sanap <email@whereistejas.xyz>")

(license NOLICENSE)

(package
 (name ocron)
 (synopsis "cron parser")
 (allow_empty)
 (depends
  ocaml
  base
  sexplib
  ppx_sexp_conv
  ppx_compare
  ppx_deriving
  ppx_let
  (ppx_inline_test :with-test)
  (ppx_assert :with-test)))
