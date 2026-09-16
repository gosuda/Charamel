let commands =
  [
    Skate_cli.get;
    Skate_cli.set;
    Skate_cli.delete;
    Skate_cli.list;
    Skate_cli.delete_db;
    Skate_cli.dbs;
  ]

let () =
  Charm_cli.run ~name:"skate" ~version:Charm_cli.Version.current
    ~doc:"Personal key-value store" commands
