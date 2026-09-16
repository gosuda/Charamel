(** Observable contract cases for lipgloss tables, trees, and lists.

    These cases encode the approved Step 6 target contract: width-aware table sizing and
    clipping, tree callback geometry, and every flat-list enumerator boundary.

    The upstream fixtures that require APIs intentionally absent from this redesign are
    not fabricated here: table border-side/header-separator/row toggles, fit-content and
    mutable data/visible-row accessors; tree hidden nodes, offsets, filters, and nested
    list features; list transforms and per-item styles. *)

val cases : unit Alcotest.test_case list
(** [cases] are the table, tree, and list rendering cases. *)
