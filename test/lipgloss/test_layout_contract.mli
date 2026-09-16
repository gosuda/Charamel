(** Observable contract cases for [Charm_lipgloss.Layout].

    The cases cover escape-aware and grapheme-aware measurements, horizontal and vertical
    joining, placement and odd-cell alignment, patterned and styled whitespace, and range-
    and grapheme-indexed styling. Geometry vectors are derived from the pinned lipgloss
    join, position, size, ranges, and runes sources. *)

val cases : unit Alcotest.test_case list
(** [cases] is the layout contract suite assembled by the lipgloss test driver. *)
