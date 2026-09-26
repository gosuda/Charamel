(** Typed terminal forms and their runners.

    [Charamel_huh] is the public namespace. The module aliases below deliberately preserve
    the private representation and type equalities of keys, results, fields, groups and
    forms; callers should construct values through the smart constructors in those
    modules. *)

module Key = Key
module Results = Results
module Dyn = Dyn
module Validate = Validate
module Keymap = Keymap
module Styles = Styles
module Theme = Theme
module Env = Env
module Field = Field_impl.Field
module Accessible = Accessible
module Group = Group
module Form = Form
module Run = Run
module Spinner = Spinner

type error = Run.error
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
(** [run] is {!Run.run}. *)
