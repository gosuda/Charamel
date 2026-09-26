(** The command-line dispatcher for the ported bubbletea examples.

    [bubbletea_examples NAME] runs the example [NAME]. On a terminal it runs the
    interactive program; without one it runs the scripted smoke instead, so the command is
    usable from a test. [bubbletea_examples --smoke NAME] always runs the smoke. The smoke
    prints [NAME: ok] with the number of assertions when every expected substring occurs
    in its frame, and prints each missing substring with the frame otherwise. A smoke that
    asserts nothing, or that raises, exits with status 1. [bubbletea_examples --list]
    prints every registered name. An unknown name exits with status 2. *)
