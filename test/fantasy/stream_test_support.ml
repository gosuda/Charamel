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
