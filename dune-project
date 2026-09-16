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
  dune-build-info
  bytesrw
  cohttp
  http
  tls
  domain-name
  cstruct
  optint
  (alcotest :with-test)))

(package
 (name charm-ssh)
 (synopsis "SSH libraries for the Charm ecosystem")
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
  cstruct
  mtime
  (alcotest :with-test)))

(package
 (name gum)
 (synopsis "Glamorous shell scripts")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  cmdliner
  eio
  fmt
  logs
  re
  ptime
  cstruct
  (eio_main :with-test)
  (alcotest :with-test)))

(package
 (name glow)
 (synopsis "Render markdown on the CLI")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  eio
  eio_main
  cmdliner
  fmt
  jsont
  cohttp-eio
  cohttp
  tls-eio
  ca-certs
  uri
  domain-name
  re
  (alcotest :with-test)))

(package
 (name freeze)
 (synopsis "Generate images of code and terminal output")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  eio
  eio_main
  cmdliner
  jsont
  cstruct
  base64
  fmt
  (alcotest :with-test)))

(package
 (name sequin)
 (synopsis "Explain terminal escape sequences")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  fmt
  eio
  eio_main
  cmdliner
  (alcotest :with-test)))

(package
 (name pop)
 (synopsis "Send mail from the command line")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  cohttp-eio
  tls-eio
  ca-certs
  cmarkit
  cohttp
  http
  tls
  domain-name
  eio
  fmt
  jsont
  base64
  digestif
  ptime
  uri
  cmdliner
  eio_main
  mirage-crypto-rng
  (alcotest :with-test)))

(package
 (name skate)
 (synopsis "Personal key-value store")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  eio
  fmt
  jsont
  base64
  cmdliner
  (eio_main :with-test)
  (alcotest :with-test)))

(package
 (name melt)
 (synopsis "Back up and restore SSH keys with mnemonics")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  charm-ssh
  digestif
  eio
  eio_main
  cmdliner
  mirage-crypto-rng
  fmt
  (alcotest :with-test)))

(package
 (name keygen)
 (synopsis "Generate OpenSSH key pairs")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  charm-ssh
  eio
  fmt
  eio_main
  cmdliner
  mirage-crypto-rng
  (alcotest :with-test)))

(package
 (name hotdiva2000)
 (synopsis "Memorable name generator")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  fmt
  cmdliner
  eio
  eio_main
  mirage-crypto-rng
  (alcotest :with-test)))

(package
 (name crush)
 (synopsis "Agentic coding harness")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charm
  cohttp-eio
  tls-eio
  ca-certs
  eio
  jsont
  bytesrw
  re
  fmt
  logs
  digestif
  base64
  cohttp
  http
  uri
  mtime
  mirage-crypto-rng
  tls
  domain-name
  cstruct
  cmdliner
  eio_main
  optint
  ipaddr
  (alcotest :with-test)))
