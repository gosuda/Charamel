open Spec

let spec =
  make_spec ~names:[ "dockerfile" ]
    ~keywords:
      [
        "FROM";
        "RUN";
        "CMD";
        "LABEL";
        "MAINTAINER";
        "EXPOSE";
        "ENV";
        "ADD";
        "COPY";
        "ENTRYPOINT";
        "VOLUME";
        "USER";
        "WORKDIR";
        "ARG";
        "ONBUILD";
        "STOPSIGNAL";
        "HEALTHCHECK";
        "SHELL";
        "AS";
      ]
    ~line_comment:[ "#" ]
    ~strings:[ ("\"", "\"", true); ("'", "'", true) ]
    ~number:integer_number ~ident:identifier_dash ~operators:[ "="; ":" ]
    ~attribute:
      (Some
         (alt
            [
              seq [ str "${"; rep (not_chars "}\n"); str "}" ];
              seq [ str "$"; identifier ];
            ]))
    ~case_sensitive:false ()
