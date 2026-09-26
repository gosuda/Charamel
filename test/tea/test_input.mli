(** Incremental terminal input decoder tests. *)

val cases : unit Alcotest.test_case list
(** [cases] exercises every supported input protocol family, chunk boundaries, timeout
    handling, raw unknown preservation, bracketed paste segmentation, byte caps, and the
    grapheme clusters of the shared corpus, one case per corpus row. *)
