open Charamel_fantasy

let finish_name = function
  | `Stop -> "Stop"
  | `Tool_calls -> "Tool_calls"
  | `Length -> "Length"
  | `Content_filter -> "Content_filter"
  | `Error message -> "Error " ^ message

let part : Stream_part.t Alcotest.testable = Alcotest.of_pp Stream_part.pp
let parts = Alcotest.list part
let last = function [] -> None | xs -> Some (List.nth xs (List.length xs - 1))
let is_finish = function Stream_part.Finish _ -> true | _ -> false

let single_finish events =
  match List.filter is_finish events with
  | [ Stream_part.Finish reason ] -> reason
  | _ -> Alcotest.failf "expected exactly one finish: %a" Fmt.(list Stream_part.pp) events

let drain stream =
  let rec loop acc =
    match Eio.Stream.take stream with
    | Stream_part.Finish _ as event -> List.rev (event :: acc)
    | event -> loop (event :: acc)
  in
  loop []

let drain_queued stream =
  let rec loop acc =
    match Eio.Stream.take_nonblocking stream with
    | None -> List.rev acc
    | Some event -> loop (event :: acc)
  in
  loop []

let parse_body = function
  | None -> Alcotest.fail "no request body was posted"
  | Some body -> (
      match Jsont_bytesrw.decode_string Jsont.json body with
      | Ok json -> json
      | Error error -> Alcotest.failf "posted body is not JSON: %s" error)

let member name (json : Jsont.json) =
  match json with
  | Jsont.Object (members, _) -> (
      match Jsont.Json.find_mem name members with
      | Some (_, value) -> Some value
      | None -> None)
  | _ -> None

let required name json =
  match member name json with
  | Some value -> value
  | None -> Alcotest.failf "JSON member %s is missing" name

let array name json =
  match required name json with
  | Jsont.Array (values, _) -> values
  | _ -> Alcotest.failf "JSON member %s is not an array" name

let object_value name json =
  match required name json with
  | Jsont.Object _ as value -> value
  | _ -> Alcotest.failf "JSON member %s is not an object" name

let string_value label json =
  match json with
  | Jsont.String (value, _) -> value
  | _ -> Alcotest.failf "%s is not a JSON string" label

let number_value label json =
  match json with
  | Jsont.Number (value, _) -> value
  | _ -> Alcotest.failf "%s is not a JSON number" label

let check_string label expected json =
  Alcotest.(check string) label expected (string_value label json)

let check_member_string label name expected json =
  check_string label expected (required name json)

let check_member_number label name expected json =
  Alcotest.(check (float 0.0001)) label expected (number_value label (required name json))

let check_bool_member label name expected json =
  let actual =
    match required name json with
    | Jsont.Bool (value, _) -> value
    | _ -> Alcotest.failf "%s is not a JSON boolean" label
  in
  Alcotest.(check bool) label expected actual

let check_string_array label expected json =
  let actual =
    match json with
    | Jsont.Array (values, _) -> List.map (string_value label) values
    | _ -> Alcotest.failf "%s is not an array" label
  in
  Alcotest.(check (list string)) label expected actual

let read_fixture ~fixture_path () =
  let inc = open_in_bin fixture_path in
  let buf = Buffer.create 8192 in
  (try
     while true do
       Buffer.add_string buf (input_line inc);
       Buffer.add_char buf '\n'
     done
   with End_of_file -> close_in inc);
  Buffer.contents buf

let expect_parts ~label expected observed = Alcotest.(check parts) label expected observed
