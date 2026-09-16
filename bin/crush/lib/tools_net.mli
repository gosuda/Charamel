(** Network fetch tool.

    HTTP and HTTPS documents are fetched through the supplied network capability with
    bounded redirects, bodies and deadlines. *)

val fetch : Tool.t
(** [fetch] retrieves an HTTP or HTTPS document and renders it as HTML, text or Markdown.
*)
