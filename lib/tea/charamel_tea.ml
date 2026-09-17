module Key = Key
module Mouse = Mouse
module Event = Event
module Cursor = Cursor
module View = View
module Cmd = Cmd
module Sub = Sub

type ('model, 'msg) app = ('model, 'msg) App.t = {
  init : unit -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> View.t;
  subscriptions : 'model -> 'msg Sub.t;
}

module Terminal = Terminal

type error = Program.error

let run = Program.run

module Test = Test
