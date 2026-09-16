(** The Elm-architecture program description [Charm_tea]'s runtime drives.

    An application is a plain record. A caller builds one directly with the fields below.
    [Charm_tea] re-exports [t] under the name [app]. *)

type ('model, 'msg) t = {
  init : unit -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> View.t;
  subscriptions : 'model -> 'msg Sub.t;
}
(** The type for an application over ['model] and ['msg].

    [init] is the starting model and its first command. [update m t] is the model after
    message [m] is applied to [t] and the command that follows from it. [view t] is the
    terminal state [t] renders as. [subscriptions t] is the set of external events [t] is
    currently interested in. *)
