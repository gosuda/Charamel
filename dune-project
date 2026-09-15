(lang dune 3.24)
(name charm)
(generate_opam_files true)
(implicit_transitive_deps false)
(formatting (enabled_for ocaml dune))
(source (github metaphorics/charm))
(license Apache-2.0)
(authors "metaphorics <152830360+metaphorics@users.noreply.github.com>")
(maintainers "metaphorics <152830360+metaphorics@users.noreply.github.com>")

(package
 (name charm)
 (synopsis "Charm ecosystem in OCaml")
 (allow_empty)
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  (eio (>= 1.5))
  eio_main
  (cmdliner (>= 2.1))
  fmt
  logs
  (jsont (>= 0.4))
  (cmarkit (>= 0.4))
  (uucp (>= 17.0.0))
  (uuseg (>= 17.0.0))
  uutf
  (re (>= 1.14))
  mtime
  ptime
  digestif
  base64
  cohttp-eio
  tls-eio
  ca-certs
  mirage-crypto-rng
  uri
  (alcotest :with-test)))

(package
 (name charm-ssh)
 (synopsis "SSH libraries for the Charm ecosystem")
 (allow_empty)
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  (eio (>= 1.5))
  eio_main
  (cmdliner (>= 2.1))
  fmt
  logs
  (jsont (>= 0.4))
  charm
  (awa (>= 0.6.1))
  (mirage-crypto (>= 2.4.1))
  mirage-crypto-ec
  mirage-crypto-rng
  x509
  base64
  digestif
  (alcotest :with-test)))

(package
 (name gum)
 (synopsis "Glamorous shell scripts")
 (allow_empty)
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  (alcotest :with-test)))

(package
 (name glow)
 (synopsis "Render markdown on the CLI")
 (allow_empty)
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  (alcotest :with-test)))

(package
 (name freeze)
 (synopsis "Generate images of code and terminal output")
 (allow_empty)
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  (alcotest :with-test)))

(package
 (name sequin)
 (synopsis "Explain terminal escape sequences")
 (allow_empty)
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  (alcotest :with-test)))

(package
 (name pop)
 (synopsis "Send mail from the command line")
 (allow_empty)
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  cohttp-eio
  tls-eio
  ca-certs
  (alcotest :with-test)))

(package
 (name skate)
 (synopsis "Personal key-value store")
 (allow_empty)
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  (alcotest :with-test)))

(package
 (name melt)
 (synopsis "Back up and restore SSH keys with mnemonics")
 (allow_empty)
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  charm-ssh
  (alcotest :with-test)))

(package
 (name keygen)
 (synopsis "Generate OpenSSH key pairs")
 (allow_empty)
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  charm-ssh
  (alcotest :with-test)))

(package
 (name hotdiva2000)
 (synopsis "Memorable name generator")
 (allow_empty)
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  (alcotest :with-test)))

(package
 (name crush)
 (synopsis "Agentic coding harness")
 (allow_empty)
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  cohttp-eio
  tls-eio
  ca-certs
  (alcotest :with-test)))
