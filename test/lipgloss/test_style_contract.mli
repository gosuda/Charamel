(** Behavior cases for {!Charm_lipgloss.Style}: property set/unset/inheritance,
    hyperlinks, exact width versus minimum height, padding/border/margin render order,
    per-edge border colors, escape payloads, zero-width borders,
    underline/strikethrough/color_whitespace, and max clipping.

    Ported from [.references/lipgloss/style_test.go] where our SGR bytes match upstream's
    byte for byte; derived from a direct trace of [Charm_ansi.Style.to_sgr] (cross-checked
    against [test/ansi/test_style.ml]'s "full style" case) where our style design produces
    different, but semantically equivalent, SGR minimization. Geometry cases use
    {!Charm_lipgloss.Layout.width}/[height] rather than raw string comparison. *)

val cases : unit Alcotest.test_case list
(** [cases] are the test cases for the {!Charm_lipgloss.Style} contract. *)
