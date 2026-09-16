module W = Charm_ssh_wish
module K = Charm_ssh_keygen

type model = { count : int }
type msg = Key of Charm_tea.Key.t

let app _session : (model, msg) Charm_tea.app =
  {
    Charm_tea.init = (fun () -> ({ count = 0 }, Charm_tea.Cmd.none));
    update =
      (fun (Key key) model ->
        match key.Charm_tea.Key.code with
        | Charm_tea.Key.Char c when Uchar.equal c (Uchar.of_char 'q') ->
            (model, Charm_tea.Cmd.quit)
        | Charm_tea.Key.Char c when Uchar.equal c (Uchar.of_char 'k') ->
            ({ count = model.count + 1 }, Charm_tea.Cmd.none)
        | _ -> (model, Charm_tea.Cmd.none));
    view =
      (fun model ->
        Charm_tea.View.v
          (Fmt.str "count: %d\n\npress k to increment, q to quit" model.count));
    subscriptions = (fun _ -> Charm_tea.Sub.key (fun key -> Key key));
  }

let error_message = function
  | `Malformed -> "malformed key"
  | `Unsupported_type -> "unsupported key type"
  | `Encrypted_key -> "encrypted key"
  | `Already_exists path -> Fmt.str "%s already exists" path
  | `Io message -> message

let run (env : Eio_unix.Stdenv.base) =
  let host_key =
    match K.load_or_generate ~fs:(fst env#fs) ~path:"id_ed25519" K.Ed25519 with
    | Ok (key, _) -> key
    | Error error -> Fmt.failwith "cannot load host key: %s" (error_message error)
  in
  let endpoint = W.logging (W.active_term ((W.tea ~env app) (fun _session -> ()))) in
  Eio.Switch.run (fun sw ->
      W.serve ~sw ~net:env#net ~clock:env#clock ~host_key
        ~addr:(`Tcp (Eio.Net.Ipaddr.V4.any, 2222))
        ~public_key_auth:(fun ~user:_ _ -> true)
        endpoint)

let () =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run run
