type ('model, 'msg) t = {
  init : unit -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> View.t;
  subscriptions : 'model -> 'msg Sub.t;
}
