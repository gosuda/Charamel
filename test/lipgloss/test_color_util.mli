(** Behavior cases for {!Charamel_lipgloss.Color_util}: alpha handling, hue rotation,
    darkening and lightening, the HSL darkness test, and profile-keyed selection.

    Ported from [.references/lipgloss/color_test.go]
    ([TestAlpha/TestComplementary/TestDarken/TestLighten]) and the upstream vectors of
    [color.go:208-360]. *)

val cases : unit Alcotest.test_case list
(** [cases] are the test cases for the {!Charamel_lipgloss.Color_util} contract. *)
