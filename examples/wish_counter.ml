module W = Charamel_ssh_wish
module K = Charamel_ssh_keygen

type model = { count : int }
type msg = Key of Charamel_tea.Key.t

let app _session : (model, msg) Charamel_tea.app =
  {
    Charamel_tea.init = (fun () -> ({ count = 0 }, Charamel_tea.Cmd.none));
    update =
      (fun (Key key) model ->
        match key.Charamel_tea.Key.code with
        | Charamel_tea.Key.Char c when Uchar.equal c (Uchar.of_char 'q') ->
            (model, Charamel_tea.Cmd.quit)
        | Charamel_tea.Key.Char c when Uchar.equal c (Uchar.of_char 'k') ->
            ({ count = model.count + 1 }, Charamel_tea.Cmd.none)
        | _ -> (model, Charamel_tea.Cmd.none));
    view =
      (fun model ->
        Charamel_tea.View.v
          (Fmt.str "count: %d\n\npress k to increment, q to quit" model.count));
    subscriptions = (fun _ -> Charamel_tea.Sub.key (fun key -> Key key));
  }

let error_message = function
  | `Malformed -> "malformed key"
  | `Unsupported_type -> "unsupported key type"
  | `Encrypted_key -> "encrypted key"
  | `Already_exists path -> Fmt.str "%s already exists" path
  | `Io message -> message

let env () =
  {
    Charamel_cli.Env.cwd = Unix.getcwd ();
    fs_root = ".";
    stdin = Lwt_io.stdin;
    stdout = Lwt_io.stdout;
    stderr = Lwt_io.stderr;
    clock = Charamel_os.Time.lwt;
  }

let run () =
  let open Lwt.Syntax in
  let* result = K.load_or_generate ~fs_root:"." ~path:"id_ed25519" K.Ed25519 in
  let host_key =
    match result with
    | Ok (key, _) -> key
    | Error error -> Fmt.failwith "cannot load host key: %s" (error_message error)
  in
  let endpoint =
    W.logging (W.active_term (W.tea ~env:(env ()) app (fun _session -> Lwt.return_unit)))
  in
  W.serve ~host_key
    ~addr:(`Tcp ("0.0.0.0", 2222))
    ~public_key_auth:(fun ~user:_ _ -> true)
    endpoint ()

let () =
  Mirage_crypto_rng_unix.use_default ();
  Lwt_main.run (run ())
