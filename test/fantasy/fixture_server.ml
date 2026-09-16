type t = {
  mutable port : int;
  responses : (int * string * (string * string) list * float option) Queue.t;
  mutable trunc_bytes : int option;
  mutable last_path : string option;
  mutable last_body : string option;
  mutable last_headers : (string * string) list;
  mutable count : int;
}

let default_content_type = [ ("content-type", "text/event-stream") ]

let reason_phrase = function
  | 200 -> "OK"
  | 201 -> "Created"
  | 204 -> "No Content"
  | 301 -> "Moved Permanently"
  | 304 -> "Not Modified"
  | 400 -> "Bad Request"
  | 401 -> "Unauthorized"
  | 403 -> "Forbidden"
  | 404 -> "Not Found"
  | 408 -> "Request Timeout"
  | 409 -> "Conflict"
  | 429 -> "Too Many Requests"
  | 500 -> "Internal Server Error"
  | 502 -> "Bad Gateway"
  | 503 -> "Service Unavailable"
  | status -> string_of_int status

(* Read request headers one line at a time until the blank line. *)
let read_headers reader =
  let rec loop acc =
    match Eio.Buf_read.line reader with
    | "" -> List.rev acc
    | line -> (
        match String.index_opt line ':' with
        | Some pos ->
            let name = String.lowercase_ascii (String.trim (String.sub line 0 pos)) in
            let value =
              String.trim (String.sub line (pos + 1) (String.length line - pos - 1))
            in
            loop ((name, value) :: acc)
        | None -> loop acc)
  in
  loop []

(* The request path from a "METHOD path HTTP/1.1" request line. *)
let path_of_request_line request_line =
  match String.index_opt request_line ' ' with
  | Some pos -> (
      let rest =
        String.sub request_line (pos + 1) (String.length request_line - pos - 1)
      in
      match String.index_opt rest ' ' with
      | Some end_pos -> Some (String.sub rest 0 end_pos)
      | None -> Some rest)
  | None -> None

let respond_one t flow reader =
  let request_line = Eio.Buf_read.line reader in
  let headers = read_headers reader in
  let content_length =
    match List.assoc_opt "content-length" headers with
    | Some len -> int_of_string len
    | None -> 0
  in
  let body = Eio.Buf_read.take content_length reader in
  (match path_of_request_line request_line with
  | Some path -> t.last_path <- Some path
  | None -> ());
  t.last_body <- Some body;
  t.last_headers <- headers;
  t.count <- t.count + 1;
  let queued = Queue.take_opt t.responses in
  match queued with
  | None ->
      Eio.Flow.copy_string
        "HTTP/1.1 500 Internal Server Error\r\ncontent-length: 0\r\n\r\n" flow;
      Eio.Flow.close flow
  | Some (status, resp_body, extra_headers, retry_after) ->
      let limit = t.trunc_bytes in
      t.trunc_bytes <- None;
      let truncated =
        match limit with Some n when n < String.length resp_body -> true | _ -> false
      in
      let sent_body =
        match limit with
        | Some n when truncated -> String.sub resp_body 0 n
        | _ -> resp_body
      in
      let retry_headers =
        match retry_after with
        | Some seconds -> [ ("retry-after", Fmt.str "%.3g" seconds) ]
        | None -> []
      in
      let header_lines =
        List.map
          (fun (k, v) -> Fmt.str "%s: %s\r\n" k v)
          ((if List.mem_assoc "content-type" extra_headers then []
            else default_content_type)
          @ retry_headers @ extra_headers)
        |> String.concat ""
      in
      let head =
        Fmt.str "HTTP/1.1 %d %s\r\n%scontent-length: %d\r\n\r\n" status
          (reason_phrase status) header_lines (String.length resp_body)
      in
      Eio.Flow.copy_string head flow;
      Eio.Flow.copy_string sent_body flow;
      Eio.Flow.close flow

let start ~sw ~net () =
  let t =
    {
      port = 0;
      responses = Queue.create ();
      trunc_bytes = None;
      last_path = None;
      last_body = None;
      last_headers = [];
      count = 0;
    }
  in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 0) in
  let socket = Eio.Net.listen net ~backlog:16 ~sw addr in
  (match Eio.Net.listening_addr socket with `Tcp (_, port) -> t.port <- port | _ -> ());
  let on_error = function
    | Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _)
    | Eio.Io (Eio.Net.E (Eio.Net.Connection_failure _), _)
    | End_of_file ->
        ()
    | exn -> raise exn
  in
  let handle flow _addr =
    let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
    respond_one t flow reader
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Net.accept_fork ~sw socket ~on_error handle
      done;
      `Stop_daemon);
  t

let base_url t = Fmt.str "http://127.0.0.1:%d" t.port

let respond t ?(status = 200) ?retry_after body =
  Queue.push (status, body, [], retry_after) t.responses

let respond_with t ~status ?retry_after ~headers body =
  Queue.push (status, body, headers, retry_after) t.responses

let queue t responses =
  Queue.clear t.responses;
  List.iter
    (fun (status, body) -> Queue.push (status, body, [], None) t.responses)
    responses

let trunc t n = t.trunc_bytes <- Some n
let last_path t = t.last_path
let last_body t = t.last_body
let last_headers t = t.last_headers
let request_count t = t.count
