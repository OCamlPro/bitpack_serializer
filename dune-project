(lang dune 2.7)

(cram enable)
(name bitpack_serializer)
(allow_approximate_merlin)
(generate_opam_files false)
(version dev)
(formatting (enabled_for ocaml reason))

(package
 (name bitpack_serializer)
 (synopsis "This library provides functions for encoding efficiently simple OCaml data.")
 (description "The library provides two main modules.\n- Buffer: defines buffers for writing and reading compressed data.\n- Lens: an easy to use API for easily defining encoders and decoders.\n")
 (depends
   (ocaml (>= 4.08.0))
   (zarith ( >= 1.10 ))
   ppx_inline_test
   ppx_expect
   odoc
   ocamlformat
  )
 )


