(lang dune 3.0)

(name SiNPle)

(generate_opam_files true)

(source
 (github PaoloRibeca/SiNPle))

(authors "Paolo Ribeca")

(maintainers "Paolo Ribeca")

(license LICENSE)

(documentation https://github.com/PaoloRibeca/SiNPle#readme)

(package
 (name SiNPle)
 (synopsis "A variant caller for high-coverage sequencing data")
 (description "A variant caller for high-coverage sequencing data")
 (depends ocaml dune)
 (tags
  (topics variants "variant calling" low-frequency bioinformatics sequencing)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
