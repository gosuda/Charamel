open Spec

let spec =
  make_spec
    ~names:[ "makefile"; "make"; "gnumakefile"; "mk"; "mak" ]
    ~keywords:
      [
        "include";
        "-include";
        "sinclude";
        "export";
        "unexport";
        "define";
        "endef";
        "ifeq";
        "ifneq";
        "ifdef";
        "ifndef";
        "else";
        "endif";
        "override";
        "vpath";
      ]
    ~constants:
      [
        ".PHONY";
        ".DEFAULT";
        ".SUFFIXES";
        ".PRECIOUS";
        ".INTERMEDIATE";
        ".SECONDARY";
        ".DELETE_ON_ERROR";
        ".IGNORE";
        ".LOW_RESOLUTION_TIME";
        ".SILENT";
        ".EXPORT_ALL_VARIABLES";
        ".NOTPARALLEL";
        ".ONESHELL";
        ".POSIX";
      ]
    ~line_comment:[ "#" ]
    ~strings:[ ("\"", "\"", true); ("'", "'", true) ]
    ~number:integer_number ~ident:identifier
    ~attribute:
      (Some
         (alt
            [
              seq [ str "$("; rep (not_chars ")"); str ")" ];
              seq [ str "${"; rep (not_chars "}"); str "}" ];
              seq [ str "$"; set "@<^?*+%$" ];
            ]))
    ~operators:[ "::="; ":="; "?="; "+="; "!="; "="; ":"; ";" ]
    ()
