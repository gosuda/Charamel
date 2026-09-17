(** Behavior cases for {!Charamel_lipgloss.Style}: property set/unset/inheritance,
    hyperlinks, exact width versus minimum height, padding/border/margin render order,
    per-edge border colors, escape payloads, zero-width borders,
    underline/strikethrough/color_whitespace, and max clipping.

    Ported from [.references/lipgloss/style_test.go] where our SGR bytes match upstream's
    byte for byte; derived from a direct trace of [Charamel_ansi.Style.to_sgr]
    (cross-checked against [test/ansi/test_style.ml]'s "full style" case) where our style
    design produces different, but semantically equivalent, SGR minimization. Geometry
    cases use {!Charamel_lipgloss.Layout.width}/[height] rather than raw string
    comparison. *)

val cases : unit Alcotest.test_case list
(** [cases] are the test cases for the {!Charamel_lipgloss.Style} contract. *)
