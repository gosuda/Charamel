(** SC-04 goal harness case.

    [run ()] runs the real [keygen] binary to write an Ed25519 OpenSSH key pair into an
    owned scratch directory, cross-checks the printed fingerprint against the real
    [ssh-keygen -l], then reruns [keygen] without [--force] on the same path and requires
    that both files are refused and left byte- and mode-identical. *)

val run : unit -> unit
