let () =
  Alcotest.fail
    "SC-04 unmet: keygen -t ed25519 -f /tmp/k writes an OpenSSH key pair whose \
     fingerprint ssh-keygen -l confirms"
