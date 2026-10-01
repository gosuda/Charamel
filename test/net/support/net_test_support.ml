open Lwt.Infix
open Charamel_net

let check_transport name message result =
  (match result with
  | Error (`Transport actual) -> Alcotest.(check string) name message actual
  | Error _ -> Alcotest.failf "%s: expected a Transport failure" name
  | Ok _ -> Alcotest.failf "%s: expected a Transport failure, got a response" name);
  Lwt.return_unit

let check_http name status title message retryable retry_after result =
  (match result with
  | Error (`Http error) ->
      Alcotest.(check int) (name ^ " status") status error.status;
      Alcotest.(check string) (name ^ " title") title error.title;
      Alcotest.(check string) (name ^ " message") message error.message;
      Alcotest.(check bool) (name ^ " retryable") retryable error.retryable;
      Alcotest.(check (option (float 0.)))
        (name ^ " retry-after") retry_after error.retry_after
  | Error _ -> Alcotest.failf "%s: expected an Http failure" name
  | Ok _ -> Alcotest.failf "%s: expected an Http failure, got a response" name);
  Lwt.return_unit

let check_ok name testable result expected =
  (match result with
  | Ok value -> Alcotest.check testable name value expected
  | Error (`Transport failure) ->
      Alcotest.failf "%s: unexpected Transport failure: %s" name failure
  | Error _ -> Alcotest.failf "%s: unexpected failure" name);
  Lwt.return_unit

let value_of name = function
  | Ok value -> value
  | Error (`Transport message) -> Alcotest.failf "%s: %s" name message
  | Error _ -> Alcotest.failf "%s: unexpected failure" name

let stream_of name result = snd (value_of name result)

let status_code name result =
  value_of name result |> fst |> Cohttp.Response.status |> Cohttp.Code.code_of_status

let collect = Lwt_stream.to_list
let events = Alcotest.(list (pair string string))

let read_events stream =
  let received = ref [] in
  let on_event (event : sse_event) = received := (event.event, event.data) :: !received in
  read_sse stream ~on_event >|= fun result -> (result, List.rev !received)
