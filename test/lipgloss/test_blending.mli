(** Behavior cases for {!Charamel_lipgloss.Blending} and the border blend it drives.

    The gradient vectors are transcribed from [.references/lipgloss/blending_test.go]
    ([TestBlend1D]) and must match byte for byte; the border cases pin how a gradient is
    sliced around a frame. *)

val cases : unit Alcotest.test_case list
(** [cases] are the test cases for the {!Charamel_lipgloss.Blending} contract. *)
