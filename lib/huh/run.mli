(** Running a form against a local terminal.

    [app] exposes the same model used by [run], so deterministic callers can drive a form
    with [Charamel_tea.Test] without opening a terminal. [run] chooses accessible prompts
    when the input is not a tty or TERM is dumb; the caller may force that mode. *)

type model = { form : Form.t; timed_out : bool }
(** The model of the form application. [timed_out] is set by the deadline command. *)

type msg = Form_msg of Form.msg | Timed_out  (** Messages accepted by {!app}. *)

val app : Form.Env.t -> ?timeout:float -> Form.t -> (model, msg) Charamel_tea.app
(** [app env ?timeout form] builds the terminal application. A positive [timeout] adds a
    deadline message; a non-positive timeout has no effect. *)

type error = [ `Aborted | `Timeout ]
(** Errors returned by {!run}. *)

val pp_error : error Fmt.t
(** [pp_error] prints [aborted] or [timed out]. *)

val run :
  ?timeout:float ->
  ?accessible:bool ->
  ?env:Form.Env.t ->
  clock:Charamel_os.Time.clock ->
  Form.t ->
  (Results.t, error) result Lwt.t
(** [run ?timeout ?accessible ?env ~clock form] runs [form] against the process's own
    standard channels. [env] defaults to explicit capabilities rooted at the working
    directory, a temporary directory selected from [TMPDIR] or
    [Filename.get_temp_dir_name ()], and the editor parsed from [$EDITOR] (falling back to
    [nano]). [timeout], when positive, applies to both accessible prompts and the terminal
    application; an expired timeout fails the returned promise with [Lwt_unix.Timeout]. *)
