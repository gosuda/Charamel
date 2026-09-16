(** SC-03 goal harness case.

    [run ()] starts a loopback HTTP fixture that speaks the OpenAI- compatible
    chat-completions wire, configures the real [crush] binary with an isolated config
    pointing at it and an isolated test API key, and runs [crush run "say hi"]. It
    requires that the real request the binary sent carries the configured model, the
    streaming flag, the Bearer test key, and the user turn, and that the fixture's
    scripted reply is printed on exit 0. No network beyond the loopback fixture is used.
*)

val run : unit -> unit
