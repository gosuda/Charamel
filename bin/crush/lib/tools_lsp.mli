(** LSP-backed agent tools.

    The values in this module expose diagnostics, navigation, symbol lookup, renaming and
    server lifecycle operations through the common crush tool interface. Every operation
    uses the LSP service carried by its context. *)

val lsp_diagnostics : Tool.t
(** [lsp_diagnostics] reports diagnostics for one file or for files touched by the current
    session. *)

val lsp_definition : Tool.t
(** [lsp_definition] reports definitions of a named symbol. *)

val lsp_references : Tool.t
(** [lsp_references] reports references of a named symbol. *)

val lsp_symbols : Tool.t
(** [lsp_symbols] reports the document symbol outline for a file. *)

val lsp_rename : Tool.t
(** [lsp_rename] renames a symbol and applies the complete workspace edit. *)

val lsp_restart : Tool.t
(** [lsp_restart] restarts one LSP server or every configured server. *)

val all : Tool.t list
(** [all] is the LSP tool set in its advertised order. *)
