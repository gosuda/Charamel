(** Incremental terminal input decoder tests. *)

val cases : unit Alcotest.test_case list
(** [cases] exercises every supported input protocol family, chunk boundaries, timeout
    handling, raw unknown preservation, bracketed paste segmentation, and byte caps. *)
