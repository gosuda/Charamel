let commands =
  [
    Choose.cmd;
    Confirm.cmd;
    File.cmd;
    Filter.cmd;
    Format.cmd;
    Input.cmd;
    Join.cmd;
    Log.cmd;
    Pager.cmd;
    Spin.cmd;
    Style_cmd.cmd;
    Table.cmd;
    Write.cmd;
    Version_cmd.cmd;
  ]

let run () =
  Charamel_cli.run ~name:"gum" ~version:Charamel_cli.Version.current
    ~doc:"A tool for glamorous shell scripts." commands

let () = run ()
