module W = Charamel_ssh_wish
module K = Charamel_ssh_keygen

type model = { frame : int }
type msg = Tick of Mtime.t | Key of Charamel_tea.Key.t

let frames = [| "✦   ·    ✧    ·   ✦"; "  ·   ✦    ·   ✧   "; "✧    ·   ✦    ·    " |]

let app _session =
  {
    Charamel_tea.init = (fun () -> ({ frame = 0 }, Charamel_tea.Cmd.none));
    update =
      (fun message model ->
        match message with
        | Tick _ ->
            ({ frame = (model.frame + 1) mod Array.length frames }, Charamel_tea.Cmd.none)
        | Key key -> (
            match key.Charamel_tea.Key.code with
            | Charamel_tea.Key.Char c when Uchar.equal c (Uchar.of_char 'q') ->
                (model, Charamel_tea.Cmd.quit)
            | _ -> (model, Charamel_tea.Cmd.none)));
    view =
      (fun model ->
        let frame = frames.(model.frame) in
        Charamel_tea.View.v (Fmt.str "  %s\n\n  press q to quit\n" frame));
    subscriptions =
      (fun _ ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.every 0.15 (fun tick -> Tick tick);
          ]);
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
  let endpoint =
    W.elapsed (W.logging (W.active_term ((W.tea ~env app) (fun _session -> ()))))
  in
  Eio.Switch.run (fun sw ->
      W.serve ~sw ~net:env#net ~clock:env#clock ~host_key
        ~addr:(`Tcp (Eio.Net.Ipaddr.V4.any, 2222))
        ~public_key_auth:(fun ~user:_ _ -> true)
        endpoint)

let () =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run run
