type t = { max : int; base : float; factor : float; max_delay : float; jitter : float }

let default : t = { max = 8; base = 0.5; factor = 2.; max_delay = 60.; jitter = 0.25 }

let retryable_status = function
  | 408 | 409 | 429 -> true
  | status -> status >= 500 && status <= 599

let non_negative_number value =
  match float_of_string_opt (String.trim value) with
  | Some number when Float.is_finite number && number >= 0. -> Some number
  | _ -> None

let http_date value =
  let months =
    [ "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" ]
  in
  let words =
    String.split_on_char ' ' (String.trim value) |> List.filter (fun word -> word <> "")
  in
  match words with
  | [ _weekday; day; month; year; time; "GMT" ] -> (
      let month = List.find_index (( = ) month) months in
      let time = String.split_on_char ':' time in
      match (int_of_string_opt day, month, int_of_string_opt year, time) with
      | Some day, Some month, Some year, [ hour; minute; second ] -> (
          match
            (int_of_string_opt hour, int_of_string_opt minute, int_of_string_opt second)
          with
          | Some hour, Some minute, Some second ->
              Ptime.of_date_time ((year, month + 1, day), ((hour, minute, second), 0))
              |> Option.map Ptime.to_float_s
          | _ -> None)
      | _ -> None)
  | _ -> None

let uniform () =
  let raw = Mirage_crypto_rng.generate 4 in
  let n =
    String.fold_left
      (fun n c -> Int64.(add (shift_left n 8) (of_int (Char.code c))))
      0L raw
  in
  Int64.to_float n /. 4294967296.

let retry_after_delay ~now ~max_delay retry_after =
  let find name =
    List.find_map
      (fun (key, value) ->
        if String.equal (String.lowercase_ascii key) name then Some value else None)
      retry_after
  in
  let from_milliseconds =
    Option.bind (find "retry-after-ms") non_negative_number
    |> Option.map (fun milliseconds -> milliseconds /. 1000.)
  in
  let from_seconds =
    Option.bind (find "retry-after") (fun value ->
        match non_negative_number value with
        | Some seconds -> Some seconds
        | None -> Option.map (fun date -> Float.max 0. (date -. now)) (http_date value))
  in
  let candidate =
    match from_milliseconds with Some _ as value -> value | None -> from_seconds
  in
  match candidate with Some delay when delay <= max_delay -> Some delay | _ -> None

let delay ?(rng = uniform) ?(now = 0.) t ~attempt ~retry_after =
  if attempt < 1 then invalid_arg "Retry.delay: attempt must be positive";
  match retry_after_delay ~now ~max_delay:t.max_delay retry_after with
  | Some delay -> delay
  | None ->
      let backoff =
        Float.min t.max_delay (t.base *. Float.pow t.factor (float_of_int (attempt - 1)))
      in
      let draw = rng () in
      if (not (Float.is_finite draw)) || draw < 0. || draw > 1. then
        invalid_arg "Retry.delay: rng outside [0,1]";
      Float.min t.max_delay (backoff *. (1. -. t.jitter +. (2. *. t.jitter *. draw)))
