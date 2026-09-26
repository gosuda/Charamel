open Spec

let spec =
  make_spec ~names:[ "diff"; "patch" ] ~block_comment:[] ~number:Re.empty ~ident:Re.empty
    ~operators:[ "+++"; "---"; "@@"; "+"; "-" ]
    ~attribute:
      (Some
         (Re.seq
            [
              Re.bol;
              Re.alt
                [
                  Re.seq [ Re.str "diff --git"; Re.rep (not_chars "\n") ];
                  Re.seq
                    [
                      Re.str "index ";
                      Re.rep1 (Re.set "0123456789abcdef");
                      Re.str "..";
                      Re.rep1 (Re.set "0123456789abcdef");
                    ];
                  Re.seq [ Re.str "+++ "; Re.rep (not_chars "\n") ];
                  Re.seq [ Re.str "--- "; Re.rep (not_chars "\n") ];
                  Re.seq
                    [
                      Re.str "@@ -";
                      Re.rep1 (Re.set "0123456789,");
                      Re.str " +";
                      Re.rep1 (Re.set "0123456789,");
                      Re.str " @@";
                    ];
                  Re.seq [ Re.str "Binary files "; Re.rep (not_chars "\n") ];
                  Re.seq [ Re.str "\\ No newline at end of file" ];
                ];
            ]))
    ()
