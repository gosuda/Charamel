(lang dune 3.24)
(name charamel)
(generate_opam_files true)
(implicit_transitive_deps false)
(formatting (enabled_for ocaml dune))
(source (github metaphorics/Charamel))
(license Apache-2.0)
(authors "metaphorics <152830360+metaphorics@users.noreply.github.com>")
(maintainers "metaphorics <152830360+metaphorics@users.noreply.github.com>")

(package
 (name charamel)
 (synopsis "Charm ecosystem in OCaml")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
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
  ca-certs
  mirage-crypto-rng
  uri
  dune-build-info
  bytesrw
  (cohttp (>= 6.0.0))
  (http (>= 6.0.0))
  tls
  x509
  domain-name
  cstruct
  optint
  (lwt (>= 6.1.2))
  lwt_direct
  (cohttp-lwt (>= 6.0.0))
  (cohttp-lwt-unix (>= 6.0.0))
  conduit-lwt-unix
  (tls-lwt (>= 2.1.3))
  (awa-mirage (>= 0.7.0))
  awa
  mirage-flow
  (ctypes (>= 0.24.0))
  (ctypes-foreign (>= 0.24.0))
  integers
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))
  (qcheck-core :with-test)
  (qcheck-alcotest (and (>= 0.91) :with-test))))

(package
 (name charamel-ssh)
 (synopsis "SSH libraries for the Charm ecosystem")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  (cmdliner (>= 2.1))
  fmt
  logs
  (jsont (>= 0.4))
  charamel
  (awa (>= 0.7.0))
  (mirage-crypto (>= 2.4.1))
  mirage-crypto-ec
  mirage-crypto-pk
  mirage-crypto-rng
  x509
  zarith
  base64
  digestif
  cstruct
  mtime
  lwt
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))
  (qcheck-core :with-test)))

(package
 (name gum)
 (synopsis "Glamorous shell scripts")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charamel
  cmdliner
  fmt
  logs
  re
  ptime
  cstruct
  lwt
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))
  (qcheck-core :with-test)))

(package
 (name glow)
 (synopsis "Render markdown on the CLI")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charamel
  cmdliner
  fmt
  jsont
  cohttp
  ca-certs
  uri
  domain-name
  re
  lwt
  cohttp-lwt
  cohttp-lwt-unix
  conduit-lwt-unix
  tls
  tls-lwt
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))
  (qcheck-core :with-test)))

(package
 (name freeze)
 (synopsis "Generate images of code and terminal output")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charamel
  cmdliner
  jsont
  cstruct
  base64
  fmt
  lwt
  uri
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))
  (qcheck-core :with-test)))

(package
 (name sequin)
 (synopsis "Explain terminal escape sequences")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charamel
  fmt
  cmdliner
  lwt
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))
  (qcheck-core :with-test)))

(package
 (name pop)
 (synopsis "Send mail from the command line")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charamel
  ca-certs
  cmarkit
  cohttp
  http
  tls
  domain-name
  fmt
  jsont
  base64
  digestif
  ptime
  uri
  cmdliner
  mirage-crypto-rng
  lwt
  tls-lwt
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))
  (qcheck-core :with-test)))

(package
 (name skate)
 (synopsis "Personal key-value store")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charamel
  fmt
  jsont
  base64
  cmdliner
  lwt
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))
  (qcheck-core :with-test)))

(package
 (name melt)
 (synopsis "Back up and restore SSH keys with mnemonics")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charamel
  charamel-ssh
  digestif
  cmdliner
  mirage-crypto-rng
  fmt
  lwt
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))
  (qcheck-core :with-test)))

(package
 (name keygen)
 (synopsis "Generate OpenSSH key pairs")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charamel
  charamel-ssh
  fmt
  cmdliner
  mirage-crypto-rng
  lwt
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))
  (qcheck-core :with-test)))

(package
 (name hotdiva2000)
 (synopsis "Memorable name generator")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charamel
  fmt
  cmdliner
  mirage-crypto-rng
  lwt
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))
  (qcheck-core :with-test)))

(package
 (name crush)
 (synopsis "Agentic coding harness")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charamel
  ca-certs
  jsont
  bytesrw
  re
  fmt
  logs
  digestif
  base64
  (cohttp (>= 6.0.0))
  (http (>= 6.0.0))
  (cohttp-lwt (>= 6.0.0))
  (cohttp-lwt-unix (>= 6.0.0))
  uri
  mtime
  mirage-crypto-rng
  tls
  domain-name
  cstruct
  cmdliner
  optint
  ipaddr
  lwt_direct
  lwt
  tls-lwt
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))
  (qcheck-core :with-test)))
