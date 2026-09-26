open Lwt.Syntax
module K = Charamel_tea.Cmd

let url = "https://charm.sh/"

type status = Response of int | Failed of string
type msg = Checked of status | Key of Charamel_tea.Key.t
type model = { status : status option }

let describe (status : int) =
  match Cohttp.Code.string_of_status (Cohttp.Code.status_of_code status) with
  | phrase -> Printf.sprintf "%d %s" status phrase
  | exception _ -> string_of_int status

let app (check : unit -> (int, string) result Lwt.t) : (model, msg) Charamel_tea.app =
  let request =
    K.map
      (fun result ->
        match result with
        | Ok status -> Checked (Response status)
        | Error message -> Checked (Failed message))
      (K.await (check ()))
  in
  {
    init = (fun () -> ({ status = None }, request));
    update =
      (fun msg model ->
        match msg with
        | Key key -> (
            match Charamel_tea.Key.to_string key with
            | "q" | "escape" | "ctrl+c" -> (model, K.quit)
            | _ -> (model, K.none))
        | Checked status -> (
            let model = { status = Some status } in
            match status with Response _ -> (model, K.quit) | Failed _ -> (model, K.none)));
    view =
      (fun model ->
        let text = "Checking " ^ url ^ "..." in
        let text =
          match model.status with
          | Some (Response status) -> text ^ " " ^ describe status
          | Some (Failed message) -> text ^ " something went wrong: " ^ message
          | None -> text
        in
        Charamel_tea.View.v (text ^ "\n"));
    subscriptions = (fun _ -> Charamel_tea.Sub.key (fun key -> Key key));
  }

let describe (response, body) =
  let status = Cohttp.Code.code_of_status (Cohttp.Response.status response) in
  let* drained = Charamel_net.read_body body in
  match drained with
  | Ok _ -> Lwt.return_ok status
  | Error error -> Lwt.return_error (Charamel_net.error_message error)

let check_url () =
  let* result =
    Charamel_net.call ~meth:`GET ~body:None ~timeout:10.0 (Uri.of_string url)
  in
  match result with
  | Ok response -> describe response
  | Error error -> Lwt.return_error (Charamel_net.error_message error)

let main () = Smoke.run_ (app check_url)
let resolved status () = Lwt.return_ok status
let failed message () = Lwt.return_error message

let smoke () =
  Smoke.expect (app (resolved 200)) [] [ "Checking https://charm.sh/..." ]
  @ Smoke.expect (app (resolved 200)) [ `Wait 0.1 ] [ "200 OK" ]
  @ Smoke.expect
      (app (failed "dial tcp: no route to host"))
      [ `Wait 0.1 ]
      [ "something went wrong: dial tcp: no route to host" ]
