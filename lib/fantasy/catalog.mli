(** The model catalog.

    [embedded] is the pinned snapshot of catwalk's [/v2/providers] response; [fetch]
    refreshes it over HTTPS and offers the response [ETag] back so an unchanged catalog is
    never downloaded again. *)

type error = [ Error.t | `Not_modified ]
(** The type for catalog failures.

    [Error.t] carries HTTP and transport failures. [`Not_modified] means the endpoint
    answered the conditional request with [304]. The cached snapshot remains current. The
    response has no body and supplies no replacement catalog. The caller retains its
    cached copy. *)

val pp_error : error Fmt.t
(** [pp_error] formats a catalog failure. *)

val embedded : Provider_info.t list
(** [embedded] is the catalog pinned at build time, in upstream registry order. *)

val fetch :
  ?base_url:string ->
  ?etag:string ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  unit ->
  (Provider_info.t list * string, error) result
(** [fetch ?base_url ?etag ~net ~clock ()] downloads the catalog.

    [base_url] is the endpoint to refresh from and defaults to
    [https://catwalk.charm.sh/v2/providers]; it is how a caller points the refresh at a
    mirror. When [etag] is a non-empty validator for the caller's cached snapshot, the
    request carries [If-None-Match]; a [304] answer is [Error `Not_modified]. On [200] the
    result is the decoded catalog together with the response's unquoted [ETag], ["" ] when
    the endpoint sent none, ready to be passed back as [?etag] on the next refresh.

    A non-[200] status, a body that does not decode as the catalog codec, a body larger
    than 10 MiB, or a transport failure is reported through the error; none of these
    raise. The exchange runs under a 30 second deadline. An [https] [base_url] requires a
    [Mirage_crypto] random generator to be installed, which is the binary's startup duty.
*)
