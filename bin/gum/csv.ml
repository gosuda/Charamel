type fields_per_record = { expected : int; actual : int; row : int }

type error =
  [ `Invalid_data of string
  | `Invalid_separator
  | `Fields_per_record of fields_per_record ]

let error_message = function
  | `Invalid_separator -> "separator must be single character"
  | `Invalid_data message -> message
  | `Fields_per_record { expected; actual; row } ->
      Fmt.str "wrong number of fields on row %d: expected %d, got %d" row expected actual

let parse ?(separator = ',') ?(lazy_quotes = false) ?(fields_per_record = 0) input =
  let input =
    if
      String.length input >= 3
      && String.get input 0 = '\xEF'
      && String.get input 1 = '\xBB'
      && String.get input 2 = '\xBF'
    then String.sub input 3 (String.length input - 3)
    else input
  in
  let rows = ref [] in
  let row = ref [] in
  let field = Buffer.create 32 in
  let expected_fields =
    ref (if fields_per_record > 0 then Some fields_per_record else None)
  in
  let state = ref `Start in
  let row_started = ref false in
  let field_started = ref false in
  let row_number = ref 1 in
  let fail message = Error (`Invalid_data message) in
  let finish_field () =
    row := Buffer.contents field :: !row;
    Buffer.clear field;
    field_started := false;
    row_started := true
  in
  let finish_row () =
    finish_field ();
    let values = List.rev !row in
    row := [];
    row_started := false;
    let actual = List.length values in
    let record_result =
      match !expected_fields with
      | None when fields_per_record < 0 -> Ok ()
      | None ->
          expected_fields := Some actual;
          Ok ()
      | Some expected when expected = actual -> Ok ()
      | Some expected ->
          Error (`Fields_per_record { expected; actual; row = !row_number })
    in
    incr row_number;
    match record_result with
    | Error _ as error -> error
    | Ok () ->
        rows := values :: !rows;
        Ok ()
  in
  let finish_newline index =
    match !state with
    | `Quoted -> Some (index, fail "invalid data provided")
    | `After_quote | `Unquoted | `Start ->
        let result = finish_row () in
        state := `Start;
        if index + 1 < String.length input && String.get input (index + 1) = '\n' then
          Some (index + 1, result)
        else Some (index, result)
  in
  let rec loop index =
    if index >= String.length input then
      begin match !state with
      | `Quoted -> fail "invalid data provided"
      | `After_quote | `Unquoted -> (
          let result = finish_row () in
          match result with Ok () -> Ok (List.rev !rows) | Error _ as error -> error)
      | `Start ->
          if !row_started || !field_started then
            let result = finish_row () in
            match result with Ok () -> Ok (List.rev !rows) | Error _ as error -> error
          else Ok (List.rev !rows)
      end
    else
      let c = String.get input index in
      match !state with
      | `Start ->
          if c = separator then begin
            finish_field ();
            field_started := true;
            loop (index + 1)
          end
          else if c = '"' then begin
            state := `Quoted;
            field_started := true;
            row_started := true;
            loop (index + 1)
          end
          else if c = '\n' || c = '\r' then
            begin match finish_newline index with
            | Some (next, Ok ()) -> loop (next + 1)
            | Some (_, (Error _ as error)) -> error
            | None -> assert false
            end
          else begin
            Buffer.add_char field c;
            field_started := true;
            row_started := true;
            state := `Unquoted;
            loop (index + 1)
          end
      | `Unquoted ->
          if c = separator then begin
            finish_field ();
            state := `Start;
            loop (index + 1)
          end
          else if c = '\n' || c = '\r' then
            begin match finish_newline index with
            | Some (next, Ok ()) -> loop (next + 1)
            | Some (_, (Error _ as error)) -> error
            | None -> assert false
            end
          else if c = '"' then
            if lazy_quotes then begin
              Buffer.add_char field c;
              loop (index + 1)
            end
            else fail "invalid data provided"
          else begin
            Buffer.add_char field c;
            loop (index + 1)
          end
      | `Quoted ->
          if c = '"' then begin
            state := `After_quote;
            loop (index + 1)
          end
          else begin
            Buffer.add_char field c;
            loop (index + 1)
          end
      | `After_quote ->
          if c = '"' then begin
            Buffer.add_char field c;
            state := `Quoted;
            loop (index + 1)
          end
          else if c = separator then begin
            finish_field ();
            state := `Start;
            loop (index + 1)
          end
          else if c = '\n' || c = '\r' then
            begin match finish_newline index with
            | Some (next, Ok ()) -> loop (next + 1)
            | Some (_, (Error _ as error)) -> error
            | None -> assert false
            end
          else if lazy_quotes then begin
            Buffer.add_char field c;
            state := `Quoted;
            loop (index + 1)
          end
          else fail "invalid data provided"
  in
  loop 0

let needs_quotes ~separator field =
  let len = String.length field in
  (len > 0 && (String.get field 0 = ' ' || String.get field (len - 1) = ' '))
  || String.contains field separator
  || String.contains field '"' || String.contains field '\r' || String.contains field '\n'

let encode_field ~separator field =
  if not (needs_quotes ~separator field) then field
  else begin
    let b = Buffer.create (String.length field + 2) in
    Buffer.add_char b '"';
    String.iter
      (fun c -> if c = '"' then Buffer.add_string b "\"\"" else Buffer.add_char b c)
      field;
    Buffer.add_char b '"';
    Buffer.contents b
  end

let write_row ~separator fields =
  let encoded = List.map (encode_field ~separator) fields in
  String.concat (String.make 1 separator) encoded ^ "\n"
