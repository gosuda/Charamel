(** Behavior cases for the surface gaps: downsampling print helpers, the public
    style-preserving wrapper, measurement getters, the scalar index basis, and the
    aggregator's underline re-export.

    The wrapper vectors restate [.references/lipgloss/wrap.go] semantics; the print
    vectors restate [writer.go] through [charamel.colorprofile]; the getter vectors
    restate [get.go:347-466]. *)

val cases : unit Alcotest.test_case list
(** [cases] are the test cases for the print, wrap, and measurement surface. *)
