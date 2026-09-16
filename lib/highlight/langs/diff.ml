open Spec

let spec =
  make_spec ~names:[ "diff"; "patch" ] ~block_comment:[] ~number:Re.empty ~ident:Re.empty
    ~operators:[ "+++"; "---"; "@@"; "+"; "-" ]
    ~attribute:
      (Some
         (seq
            [
              Re.bol;
              alt
                [
                  seq [ str "diff --git"; rep (not_chars "\n") ];
                  seq
                    [
                      str "index ";
                      rep1 (set "0123456789abcdef");
                      str "..";
                      rep1 (set "0123456789abcdef");
                    ];
                  seq [ str "+++ "; rep (not_chars "\n") ];
                  seq [ str "--- "; rep (not_chars "\n") ];
                  seq
                    [
                      str "@@ -";
                      rep1 (set "0123456789,");
                      str " +";
                      rep1 (set "0123456789,");
                      str " @@";
                    ];
                  seq [ str "Binary files "; rep (not_chars "\n") ];
                  seq [ str "\\ No newline at end of file" ];
                ];
            ]))
    ()
