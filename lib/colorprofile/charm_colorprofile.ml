type t = No_tty | Ascii | Ansi | Ansi256 | True_color
type profile = t

let has_prefix s prefix = String.starts_with ~prefix s
let has_suffix s suffix = String.ends_with ~suffix s

let has_substring s needle =
  let length = String.length s in
  let needle_length = String.length needle in
  let found = ref false in
  let index = ref 0 in
  while (not !found) && !index + needle_length <= length do
    if String.sub s !index needle_length = needle then found := true;
    incr index
  done;
  !found

let truthy env name =
  match env name with Some value -> value <> "" && value <> "0" | None -> false

let color_terminal term colorterm =
  let colorterm = String.lowercase_ascii colorterm in
  let p = ref Ansi in
  if has_prefix term "tmux" || has_prefix term "screen" || has_suffix term "256color" then
    p := Ansi256;
  if
    has_substring term "alacritty"
    || has_substring term "contour" || has_substring term "foot"
    || has_substring term "ghostty" || has_substring term "kitty"
    || has_substring term "rio" || has_substring term "st" || has_substring term "wezterm"
    || has_suffix term "direct"
  then p := True_color;
  if
    (colorterm = "truecolor" || colorterm = "24bit" || colorterm = "yes"
   || colorterm = "true")
    && (not (has_prefix term "screen"))
    && not (has_prefix term "tmux")
  then p := True_color;
  !p

let detect ~is_tty ~env =
  let tty = is_tty || truthy env "TTY_FORCE" in
  let term = env "TERM" in
  let base =
    match term with
    | None | Some ("" | "dumb") -> No_tty
    | Some _ when not tty -> No_tty
    | Some term_value ->
        let colorterm = Option.value ~default:"" (env "COLORTERM") in
        let profile = color_terminal term_value colorterm in
        if Option.is_some (env "WT_SESSION") || truthy env "GOOGLE_CLOUD_SHELL" then
          True_color
        else profile
  in
  if tty && truthy env "NO_COLOR" then min base Ascii
  else if truthy env "CLICOLOR_FORCE" then max base Ansi
  else if tty && term <> Some "dumb" && truthy env "CLICOLOR" then max base Ansi
  else base

let convert profile color =
  match profile with
  | No_tty | Ascii -> Charm_ansi.Color.Default
  | Ansi -> Charm_ansi.Color.to_ansi16 color
  | Ansi256 -> Charm_ansi.Color.to_ansi256 color
  | True_color -> color

module Writer = struct
  type string_kind = Osc | Dcs | Sos | Pm | Apc

  type pending_kind =
    | Escape
    | Escape_intermediate
    | Csi
    | String_sequence of string_kind

  type pending = {
    mutable kind : pending_kind;
    bytes : Buffer.t;
    mutable string_esc : bool;
    mutable opaque : bool;
  }

  type nonrec t = {
    profile : profile;
    sink : Eio.Flow.sink_ty Eio.Resource.t;
    mutable pending : pending option;
    mutable utf8_remaining : int;
  }

  let max_sequence_size = 65536

  let start_pending t kind byte =
    let bytes = Buffer.create 32 in
    Buffer.add_char bytes (Char.chr byte);
    t.pending <- Some { kind; bytes; string_esc = false; opaque = false }

  let append_pending out pending byte =
    if pending.opaque then Buffer.add_char out (Char.chr byte)
    else if Buffer.length pending.bytes < max_sequence_size then
      Buffer.add_char pending.bytes (Char.chr byte)
    else begin
      Buffer.add_buffer out pending.bytes;
      Buffer.clear pending.bytes;
      pending.opaque <- true;
      Buffer.add_char out (Char.chr byte)
    end

  let parse_nat value =
    let length = String.length value in
    if length = 0 then None
    else
      let result = ref 0 in
      let valid = ref true in
      let index = ref 0 in
      while !valid && !index < length do
        match String.get value !index with
        | '0' .. '9' as c ->
            let digit = Char.code c - Char.code '0' in
            if !result > (max_int - digit) / 10 then valid := false
            else begin
              result := (!result * 10) + digit;
              incr index
            end
        | _ -> valid := false
      done;
      if !valid then Some !result else None

  type parameter = { raw : string; fields : string list }
  type slot = Foreground | Background | Underline
  type colour = { slot : slot; colour : Charm_ansi.Color.t; consumed : int }

  let parse_parameters body =
    let raw = String.split_on_char ';' body in
    if List.length raw > 32 then None
    else
      Some
        (Array.of_list
           (List.map (fun raw -> { raw; fields = String.split_on_char ':' raw }) raw))

  let single_number parameter =
    match parameter.fields with [ field ] -> parse_nat field | _ -> None

  let slot_of_number = function
    | 38 -> Some Foreground
    | 48 -> Some Background
    | 58 -> Some Underline
    | _ -> None

  let rgb r g b = Charm_ansi.Color.Rgb (r, g, b)

  let colour_at parameters index =
    let parameter = parameters.(index) in
    match parameter.fields with
    | first :: mode :: rest ->
        begin match parse_nat first with
        | None -> None
        | Some slot_number ->
            begin match (slot_of_number slot_number, parse_nat mode) with
            | Some slot, Some 5 ->
                begin match rest with
                | [ value ] ->
                    begin match parse_nat value with
                    | Some value ->
                        Some
                          { slot; colour = Charm_ansi.Color.Indexed value; consumed = 1 }
                    | None -> None
                    end
                | _ -> None
                end
            | Some slot, Some 2 ->
                begin match rest with
                | [ ""; r; g; b ] | [ r; g; b ] ->
                    begin match (parse_nat r, parse_nat g, parse_nat b) with
                    | Some r, Some g, Some b ->
                        Some { slot; colour = rgb r g b; consumed = 1 }
                    | _ -> None
                    end
                | _ -> None
                end
            | _ -> None
            end
        end
    | [ first ] ->
        begin match parse_nat first with
        | Some first ->
            begin match slot_of_number first with
            | None -> None
            | Some slot when index + 1 < Array.length parameters ->
                begin match single_number parameters.(index + 1) with
                | Some 5 when index + 2 < Array.length parameters ->
                    begin match single_number parameters.(index + 2) with
                    | Some value ->
                        Some
                          { slot; colour = Charm_ansi.Color.Indexed value; consumed = 3 }
                    | None -> None
                    end
                | Some 2 when index + 4 < Array.length parameters ->
                    begin match
                      ( single_number parameters.(index + 2),
                        single_number parameters.(index + 3),
                        single_number parameters.(index + 4) )
                    with
                    | Some r, Some g, Some b ->
                        Some { slot; colour = rgb r g b; consumed = 5 }
                    | _ -> None
                    end
                | _ -> None
                end
            | Some _ -> None
            end
        | None -> None
        end
    | [] -> None

  let clamp_palette n = max 0 (min 255 n)

  let emit_colour slot colour =
    let basic n = clamp_palette n in
    match (slot, colour) with
    | Foreground, Charm_ansi.Color.Basic n ->
        if n < 8 then string_of_int (30 + n) else string_of_int (82 + n)
    | Background, Charm_ansi.Color.Basic n ->
        if n < 8 then string_of_int (40 + n) else string_of_int (92 + n)
    | Underline, Charm_ansi.Color.Basic n -> "58;5;" ^ string_of_int (clamp_palette n)
    | Foreground, Charm_ansi.Color.Indexed n -> "38;5;" ^ string_of_int (clamp_palette n)
    | Background, Charm_ansi.Color.Indexed n -> "48;5;" ^ string_of_int (clamp_palette n)
    | Underline, Charm_ansi.Color.Indexed n -> "58;5;" ^ string_of_int (clamp_palette n)
    | Foreground, Charm_ansi.Color.Rgb (r, g, b) ->
        "38;2;"
        ^ string_of_int (basic r)
        ^ ";"
        ^ string_of_int (basic g)
        ^ ";"
        ^ string_of_int (basic b)
    | Background, Charm_ansi.Color.Rgb (r, g, b) ->
        "48;2;"
        ^ string_of_int (basic r)
        ^ ";"
        ^ string_of_int (basic g)
        ^ ";"
        ^ string_of_int (basic b)
    | Underline, Charm_ansi.Color.Rgb (r, g, b) ->
        "58;2;"
        ^ string_of_int (basic r)
        ^ ";"
        ^ string_of_int (basic g)
        ^ ";"
        ^ string_of_int (basic b)
    | _, Charm_ansi.Color.Default -> ""

  let transform_sgr profile raw =
    let length = String.length raw in
    let offset = if length >= 2 && String.get raw 0 = '\027' then 2 else 1 in
    if length <= offset || String.get raw (length - 1) <> 'm' then raw
    else
      let body = String.sub raw offset (length - offset - 1) in
      let valid_body = ref true in
      String.iter
        (function '0' .. '9' | ':' | ';' -> () | _ -> valid_body := false)
        body;
      if not !valid_body then raw
      else
        match parse_parameters body with
        | None -> raw
        | Some parameters ->
            if profile = No_tty || profile = Ascii then ""
            else
              let changed = ref false in
              let pieces = ref [] in
              let add piece = pieces := piece :: !pieces in
              let rec loop index =
                if index < Array.length parameters then
                  match colour_at parameters index with
                  | Some colour ->
                      changed := true;
                      add (emit_colour colour.slot (convert profile colour.colour));
                      loop (index + colour.consumed)
                  | None ->
                      add parameters.(index).raw;
                      loop (index + 1)
              in
              loop 0;
              if not !changed then raw
              else
                let prefix = if String.get raw 0 = '\027' then "\027[" else "\155" in
                prefix ^ String.concat ";" (List.rev !pieces) ^ "m"

  let finish_pending t out =
    match t.pending with
    | None -> ()
    | Some pending ->
        if not pending.opaque then begin
          let raw = Buffer.contents pending.bytes in
          match pending.kind with
          | Csi -> Buffer.add_string out (transform_sgr t.profile raw)
          | Escape | Escape_intermediate | String_sequence _ -> Buffer.add_string out raw
        end;
        t.pending <- None;
        t.utf8_remaining <- 0

  let is_lead byte =
    if byte >= 0xc2 && byte <= 0xdf then 1
    else if byte >= 0xe0 && byte <= 0xef then 2
    else if byte >= 0xf0 && byte <= 0xf4 then 3
    else 0

  let is_continuation byte = byte >= 0x80 && byte <= 0xbf
  let string_terminator kind byte = byte = 0x9c || (kind = Osc && byte = 0x07)

  let rec process_byte t out byte =
    match t.pending with
    | None ->
        if t.utf8_remaining > 0 then
          if is_continuation byte then begin
            Buffer.add_char out (Char.chr byte);
            t.utf8_remaining <- t.utf8_remaining - 1
          end
          else begin
            t.utf8_remaining <- 0;
            process_byte t out byte
          end
        else if byte = 0x1b then start_pending t Escape byte
        else if byte = 0x9b then start_pending t Csi byte
        else
          begin match byte with
          | 0x90 -> start_pending t (String_sequence Dcs) byte
          | 0x98 -> start_pending t (String_sequence Sos) byte
          | 0x9d -> start_pending t (String_sequence Osc) byte
          | 0x9e -> start_pending t (String_sequence Pm) byte
          | 0x9f -> start_pending t (String_sequence Apc) byte
          | _ ->
              Buffer.add_char out (Char.chr byte);
              t.utf8_remaining <- is_lead byte
          end
    | Some pending ->
        begin match pending.kind with
        | Escape ->
            if byte = 0x1b then begin
              finish_pending t out;
              process_byte t out byte
            end
            else begin
              append_pending out pending byte;
              if byte = 0x5b then pending.kind <- Csi
              else if byte = 0x50 then pending.kind <- String_sequence Dcs
              else if byte = 0x58 then pending.kind <- String_sequence Sos
              else if byte = 0x5d then pending.kind <- String_sequence Osc
              else if byte = 0x5e then pending.kind <- String_sequence Pm
              else if byte = 0x5f then pending.kind <- String_sequence Apc
              else if byte >= 0x20 && byte <= 0x2f then
                pending.kind <- Escape_intermediate
              else finish_pending t out
            end
        | Escape_intermediate ->
            if byte = 0x1b then begin
              finish_pending t out;
              process_byte t out byte
            end
            else begin
              append_pending out pending byte;
              if byte < 0x20 || byte > 0x2f then finish_pending t out
            end
        | Csi ->
            if byte = 0x1b then begin
              finish_pending t out;
              process_byte t out byte
            end
            else begin
              append_pending out pending byte;
              if byte >= 0x40 && byte <= 0x7e then finish_pending t out
            end
        | String_sequence kind ->
            if t.utf8_remaining > 0 && is_continuation byte then begin
              append_pending out pending byte;
              t.utf8_remaining <- t.utf8_remaining - 1
            end
            else begin
              if t.utf8_remaining > 0 then t.utf8_remaining <- 0;
              if pending.string_esc then begin
                pending.string_esc <- false;
                append_pending out pending byte;
                if byte = 0x5c || string_terminator kind byte then finish_pending t out
                else if byte = 0x1b then pending.string_esc <- true
              end
              else if byte = 0x1b then begin
                append_pending out pending byte;
                pending.string_esc <- true
              end
              else begin
                append_pending out pending byte;
                if string_terminator kind byte then finish_pending t out
                else t.utf8_remaining <- is_lead byte
              end
            end
        end

  let create ~profile sink = { profile; sink; pending = None; utf8_remaining = 0 }

  let write t text =
    if t.profile = True_color then Eio.Flow.copy_string text t.sink
    else begin
      let output = Buffer.create (String.length text) in
      String.iter (fun byte -> process_byte t output (Char.code byte)) text;
      if Buffer.length output > 0 then
        Eio.Flow.copy_string (Buffer.contents output) t.sink
    end
end
