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
  cohttp
  http
  tls
  domain-name
  cstruct
  optint
  (lwt (>= 6.1.2))
  lwt_direct
  cohttp-lwt
  cohttp-lwt-unix
  (tls-lwt (>= 2.1.3))
  (awa-mirage (>= 0.6.1))
  (ctypes (and (>= 0.24.0) (= :os "win32")))
  (ctypes-foreign (and (>= 0.24.0) (= :os "win32")))
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))))

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
  (awa (>= 0.6.1))
  (mirage-crypto (>= 2.4.1))
  mirage-crypto-ec
  mirage-crypto-rng
  x509
  base64
  digestif
  cstruct
  mtime
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))))

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
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))))

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
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))))

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
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))))

(package
 (name sequin)
 (synopsis "Explain terminal escape sequences")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.24))
  charamel
  fmt
  cmdliner
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))))

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
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))))

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
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))))

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
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))))

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
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))))

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
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))))

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
  cohttp
  http
  uri
  mtime
  mirage-crypto-rng
  tls
  domain-name
  cstruct
  cmdliner
  optint
  ipaddr
  (alcotest :with-test)
  (alcotest-lwt (and (>= 1.9.1) :with-test))))
