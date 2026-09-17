(* Behavior re-derived from .references/sequin (MIT) handlers.go, sgr.go, mode.go,
   cursor.go, screen.go, line.go, kitty.go, xt.go, color.go, hyperlink.go, title.go,
   clipboard.go, notify.go and termcap.go, plus the DEC private mode table and SGR mouse
   report format documented in xterm's ctlseqs.txt. No bytes are transcribed; only the
   shape of each explanation is redone in OCaml. Control mnemonics are the standard
   ECMA-48 abbreviations. *)

module Parser = Charamel_ansi.Parser
module Color = Charamel_ansi.Color
module Seq = Charamel_ansi.Seq

let ctrl_name code =
  match code with
  | 0 -> "NUL"
  | 1 -> "SOH"
  | 2 -> "STX"
  | 3 -> "ETX"
  | 4 -> "EOT"
  | 5 -> "ENQ"
  | 6 -> "ACK"
  | 7 -> "BEL"
  | 8 -> "BS"
  | 9 -> "TAB"
  | 10 -> "LF"
  | 11 -> "VT"
  | 12 -> "FF"
  | 13 -> "CR"
  | 14 -> "SO"
  | 15 -> "SI"
  | 16 -> "DLE"
  | 17 -> "DC1"
  | 18 -> "DC2"
  | 19 -> "DC3"
  | 20 -> "DC4"
  | 21 -> "NAK"
  | 22 -> "SYN"
  | 23 -> "ETB"
  | 24 -> "CAN"
  | 25 -> "EM"
  | 26 -> "SUB"
  | 27 -> "ESC"
  | 28 -> "FS"
  | 29 -> "GS"
  | 30 -> "RS"
  | 31 -> "US"
  | 127 -> "DEL"
  | 0x80 -> "PAD"
  | 0x81 -> "HOP"
  | 0x82 -> "BPH"
  | 0x83 -> "NBH"
  | 0x84 -> "IND"
  | 0x85 -> "NEL"
  | 0x86 -> "SSA"
  | 0x87 -> "ESA"
  | 0x88 -> "HTS"
  | 0x89 -> "HTJ"
  | 0x8a -> "VTS"
  | 0x8b -> "PLD"
  | 0x8c -> "PLU"
  | 0x8d -> "RI"
  | 0x8e -> "SS2"
  | 0x8f -> "SS3"
  | 0x90 -> "DCS"
  | 0x91 -> "PU1"
  | 0x92 -> "PU2"
  | 0x93 -> "STS"
  | 0x94 -> "CCH"
  | 0x95 -> "MW"
  | 0x96 -> "SPA"
  | 0x97 -> "EPA"
  | 0x98 -> "SOS"
  | 0x99 -> "SGC"
  | 0x9a -> "SCI"
  | 0x9b -> "CSI"
  | 0x9c -> "ST"
  | 0x9d -> "OSC"
  | 0x9e -> "PM"
  | 0x9f -> "APC"
  | n -> Fmt.str "0x%02X" n

let quote s = Fmt.str "%S" s

(* [intermediates] mixes the private prefix bytes ([?<>=]) with the true intermediate
   bytes (0x20-0x2F); the two ranges never overlap, so a single pass sorts them back
   apart. *)
let is_prefix_char = function '?' | '<' | '>' | '=' -> true | _ -> false

let split_intermediates s =
  let prefix = Buffer.create 2 and inter = Buffer.create 2 in
  String.iter
    (fun c ->
      if is_prefix_char c then Buffer.add_char prefix c else Buffer.add_char inter c)
    s;
  (Buffer.contents prefix, Buffer.contents inter)

let param_group_to_string group =
  String.concat ":" (List.map (function Some n -> string_of_int n | None -> "") group)

let params_to_string params = String.concat ";" (List.map param_group_to_string params)

(* Reconstructs the sequence body (params, intermediates and final byte) for CSI, DCS and
   ESC actions. A single space separates the params from the final byte only when there is
   no true intermediate byte to carry that separation itself, so "38;2;255;0;0m" reads as
   "38;2;255;0;0 m" while "0 q" (DECSCUSR, whose intermediate is a literal space) is not
   doubled to "0  q". *)
let csi_body ~params ~intermediates ~final =
  let prefix, inter = split_intermediates intermediates in
  let body = prefix ^ params_to_string params ^ inter in
  let sep = if inter = "" && body <> "" then " " else "" in
  Fmt.str "%s%s%c" body sep final

let param_at params idx default =
  match List.nth_opt params idx with
  | None | Some [] -> default
  | Some (v :: _) -> Option.value v ~default

(* [positive params idx default] is the declared value at [idx] when it is a positive
   integer, and [default] otherwise (absent, empty, zero or negative), matching the
   "count defaults to 1 unless a positive count is given" rule shared by cursor movement
   and line operations. *)
let positive params idx default =
  let n = param_at params idx default in
  if n > 0 then n else default

let basic_color_name = function
  | 0 -> "black"
  | 1 -> "red"
  | 2 -> "green"
  | 3 -> "yellow"
  | 4 -> "blue"
  | 5 -> "magenta"
  | 6 -> "cyan"
  | 7 -> "white"
  | _ -> "unknown"

(* Reads an SGR extended color spec (mode 38, 48 or 58) starting at [group], the top-level
   parameter group holding the introducer. Two wire forms exist: the colon form packs the
   introducer, mode and components into one group ("38:2::255:0:0", the trailing empty
   slot an optional colour-space id); the legacy semicolon form spreads them across
   separate top-level groups ("38;2;255;0;0"). Returns the decoded color, if any, and the
   groups still to process after the ones this spec consumed. *)
let read_color_spec group rest =
  let head g = match g with v :: _ -> Option.value v ~default:0 | [] -> 0 in
  match group with
  | _ :: (_ :: _ as sub) -> (
      match sub with
      | Some 5 :: tail ->
          ( Color.indexed (match tail with v :: _ -> Option.value v ~default:0 | [] -> 0),
            rest )
      | Some 2 :: comps -> (
          let n = List.length comps in
          let last3 =
            if n > 3 then List.filteri (fun i _ -> i >= n - 3) comps else comps
          in
          match last3 with
          | [ r; g; b ] ->
              let v = Option.value ~default:0 in
              (Color.rgb (v r) (v g) (v b), rest)
          | _ -> (None, rest))
      | _ -> (None, rest))
  | [ _ ] -> (
      match rest with
      | mode_group :: tail when head mode_group = 5 -> (
          match tail with
          | n_group :: tail2 -> (Color.indexed (head n_group), tail2)
          | [] -> (None, []))
      | mode_group :: tail when head mode_group = 2 -> (
          match tail with
          | r_group :: g_group :: b_group :: tail2 ->
              (Color.rgb (head r_group) (head g_group) (head b_group), tail2)
          | _ -> (None, rest))
      | _ -> (None, rest))
  | [] -> (None, rest)

let underline_style_name = function
  | 0 -> "no underline"
  | 1 -> "single underline"
  | 2 -> "double underline"
  | 3 -> "curly underline"
  | 4 -> "dotted underline"
  | 5 -> "dashed underline"
  | _ -> "underline"

let rec sgr_parts groups =
  match groups with
  | [] -> []
  | group :: rest -> (
      let code = match group with v :: _ -> Option.value v ~default:0 | [] -> 0 in
      let color_part ~label =
        let color, rest' = read_color_spec group rest in
        let part =
          match color with
          | Some c -> Fmt.str "%s color: %a" label Color.pp c
          | None -> Fmt.str "%s color: unknown" label
        in
        part :: sgr_parts rest'
      in
      match code with
      | 0 -> "reset style" :: sgr_parts rest
      | 1 -> "bold" :: sgr_parts rest
      | 2 -> "faint" :: sgr_parts rest
      | 3 -> "italic" :: sgr_parts rest
      | 4 ->
          let style =
            match group with _ :: v :: _ -> Option.value v ~default:1 | _ -> 1
          in
          underline_style_name style :: sgr_parts rest
      | 5 | 6 -> "blink" :: sgr_parts rest
      | 7 -> "reverse" :: sgr_parts rest
      | 8 -> "conceal" :: sgr_parts rest
      | 9 -> "strike" :: sgr_parts rest
      | 22 -> "normal intensity" :: sgr_parts rest
      | 23 -> "no italic" :: sgr_parts rest
      | 24 -> "no underline" :: sgr_parts rest
      | 25 -> "no blink" :: sgr_parts rest
      | 27 -> "no reverse" :: sgr_parts rest
      | 28 -> "no conceal" :: sgr_parts rest
      | 29 -> "no strike" :: sgr_parts rest
      | (30 | 31 | 32 | 33 | 34 | 35 | 36 | 37) as c ->
          Fmt.str "foreground color: %s" (basic_color_name (c - 30)) :: sgr_parts rest
      | 38 -> color_part ~label:"foreground"
      | 39 -> "default foreground color" :: sgr_parts rest
      | (40 | 41 | 42 | 43 | 44 | 45 | 46 | 47) as c ->
          Fmt.str "background color: %s" (basic_color_name (c - 40)) :: sgr_parts rest
      | 48 -> color_part ~label:"background"
      | 49 -> "default background color" :: sgr_parts rest
      | 58 -> color_part ~label:"underline"
      | 59 -> "default underline color" :: sgr_parts rest
      | (90 | 91 | 92 | 93 | 94 | 95 | 96 | 97) as c ->
          Fmt.str "foreground color: bright %s" (basic_color_name (c - 90))
          :: sgr_parts rest
      | (100 | 101 | 102 | 103 | 104 | 105 | 106 | 107) as c ->
          Fmt.str "background color: bright %s" (basic_color_name (c - 100))
          :: sgr_parts rest
      | _ -> "unknown" :: sgr_parts rest)

let describe_sgr params =
  if params = [] then "SGR: reset style"
  else "SGR: " ^ String.concat ", " (sgr_parts params)

let cursor_style_name = function
  | 0 | 1 -> "blinking block"
  | 2 -> "steady block"
  | 3 -> "blinking underline"
  | 4 -> "steady underline"
  | 5 -> "blinking bar"
  | 6 -> "steady bar"
  | _ -> "unknown"

let describe_erase_display ~selective n =
  let base =
    match n with
    | 0 -> Some "Erase screen below cursor"
    | 1 -> Some "Erase screen above cursor"
    | 2 -> Some "Erase entire screen"
    | 3 -> Some "Erase entire screen and scrollback"
    | _ -> None
  in
  match base with
  | Some s -> if selective then s ^ " (selective)" else s
  | None -> "Unknown"

let describe_erase_line ~selective n =
  let base =
    match n with
    | 0 -> Some "Erase line right of cursor"
    | 1 -> Some "Erase line left of cursor"
    | 2 -> Some "Erase entire line"
    | _ -> None
  in
  match base with
  | Some s -> if selective then s ^ " (selective)" else s
  | None -> "Unknown"

(* DEC private mode names grounded in .references/sequin/mode.go and the numbers
   Charamel_ansi.Seq exposes; a mode not in either grounded set explains as unknown rather
   than guessed. *)
let private_mode_name m =
  match m with
  | m when m = Seq.alt_screen -> Some "alternate screen"
  | m when m = Seq.cursor_visible -> Some "cursor visibility"
  | m when m = Seq.mouse_click -> Some "mouse click"
  | m when m = Seq.mouse_motion -> Some "mouse cell motion"
  | m when m = Seq.mouse_all -> Some "mouse all motion"
  | m when m = Seq.mouse_sgr -> Some "mouse SGR encoding"
  | m when m = Seq.mouse_pixels -> Some "mouse SGR pixel encoding"
  | m when m = Seq.bracketed_paste -> Some "bracketed paste"
  | m when m = Seq.focus -> Some "focus reporting"
  | m when m = Seq.sync_output -> Some "synchronized output"
  | m when m = Seq.grapheme_clustering -> Some "grapheme clustering"
  | 1 -> Some "cursor keys"
  | 1001 -> Some "mouse hilite"
  | 9001 -> Some "win32 input"
  | _ -> None

let standard_mode_name = function 4 -> Some "insert/replace mode" | _ -> None

type mode_action = Mode_request | Mode_enable | Mode_disable

let mode_label ~private_ mode =
  match if private_ then private_mode_name mode else standard_mode_name mode with
  | Some name -> Fmt.str "%s (%d)" name mode
  | None -> Fmt.str "%d (unknown)" mode

let describe_mode ~action ~private_ mode =
  let scope = if private_ then "private mode" else "mode" in
  let label = mode_label ~private_ mode in
  match action with
  | Mode_request -> Fmt.str "Request %s %s" scope label
  | Mode_enable -> Fmt.str "Enable %s %s" scope label
  | Mode_disable -> Fmt.str "Disable %s %s" scope label

let kitty_flag_desc flags =
  let bits =
    [
      (1, "disambiguate escape codes");
      (2, "report event types");
      (4, "report alternate keys");
      (8, "report all keys as escape codes");
      (16, "report associated text");
    ]
  in
  let set =
    List.filter_map
      (fun (bit, name) -> if flags land bit <> 0 then Some name else None)
      bits
  in
  if set = [] then "no flags" else String.concat ", " set

let kitty_mode_desc = function
  | 1 -> "replacing existing flags"
  | 2 -> "adding to existing flags"
  | 3 -> "removing from existing flags"
  | _ -> "unknown mode"

let describe_kitty ~prefix params =
  let p0 = param_at params 0 0 in
  if String.contains prefix '?' then "Request Kitty keyboard flags"
  else if String.contains prefix '>' then
    if p0 = 0 then "Disable Kitty keyboard"
    else Fmt.str "Push Kitty keyboard flags: %s" (kitty_flag_desc p0)
  else if String.contains prefix '<' then Fmt.str "Pop %d Kitty keyboard flag(s)" p0
  else if String.contains prefix '=' then
    match List.nth_opt params 1 with
    | Some (Some m :: _) ->
        Fmt.str "Set Kitty keyboard flags to %s (%s)" (kitty_flag_desc p0)
          (kitty_mode_desc m)
    | _ -> Fmt.str "Set Kitty keyboard flags to %s" (kitty_flag_desc p0)
  else "Unknown"

(* SGR mouse report (mode 1006), documented in xterm's ctlseqs.txt: CSI < Cb ; Cx ; Cy M/m.
   Bit 2 (4) is shift, bit 3 (8) is meta, bit 4 (16) is ctrl, bit 5 (32) is motion, and bit
   6 (64) switches the low two bits from a button number to a wheel direction. *)
let mouse_button_name ~wheel base =
  if wheel then
    match base with
    | 0 -> "wheel up"
    | 1 -> "wheel down"
    | 2 -> "wheel left"
    | 3 -> "wheel right"
    | _ -> "unknown wheel"
  else
    match base with
    | 0 -> "left"
    | 1 -> "middle"
    | 2 -> "right"
    | 3 -> "none"
    | _ -> "unknown"

let describe_mouse ~final params =
  match params with
  | [ b_grp; x_grp; y_grp ] ->
      let v g = match g with Some n :: _ -> n | _ -> 0 in
      let b = v b_grp and x = v x_grp and y = v y_grp in
      let wheel = b land 64 <> 0 in
      let motion = b land 32 <> 0 in
      let button = mouse_button_name ~wheel (b land 0b11) in
      let mods =
        List.filter_map
          (fun (bit, name) -> if b land bit <> 0 then Some name else None)
          [ (4, "shift"); (8, "meta"); (16, "ctrl") ]
      in
      let mods_str =
        if mods = [] then "" else Fmt.str " modifiers=%s" (String.concat "+" mods)
      in
      let action =
        match (final = 'M', motion) with
        | true, true -> "motion"
        | true, false -> "press"
        | false, _ -> "release"
      in
      Fmt.str "Mouse %s button=%s%s x=%d y=%d" action button mods_str x y
  | _ -> "Unknown"

let describe_csi ~params ~intermediates ~final =
  let prefix, inter = split_intermediates intermediates in
  let has c = String.contains prefix c in
  let has_inter c = String.contains inter c in
  if has '<' && (final = 'M' || final = 'm') then describe_mouse ~final params
  else
    match final with
    | 'm' -> describe_sgr params
    | 'c' when prefix = "" && inter = "" -> "Request primary device attributes"
    | 'q' when has '>' ->
        if param_at params 0 0 = 0 then "Request terminal name and version" else "Unknown"
    | 'q' when has_inter ' ' ->
        Fmt.str "Set cursor style: %s" (cursor_style_name (positive params 0 1))
    | 'u' when prefix <> "" -> describe_kitty ~prefix params
    | 'u' -> "Restore cursor position"
    | 'A' -> Fmt.str "Cursor up %d" (positive params 0 1)
    | 'B' -> Fmt.str "Cursor down %d" (positive params 0 1)
    | 'C' -> Fmt.str "Cursor right %d" (positive params 0 1)
    | 'D' -> Fmt.str "Cursor left %d" (positive params 0 1)
    | 'E' -> Fmt.str "Cursor down %d, column 1" (positive params 0 1)
    | 'F' -> Fmt.str "Cursor up %d, column 1" (positive params 0 1)
    | 'H' ->
        Fmt.str "Set cursor position row=%d col=%d" (positive params 0 1)
          (positive params 1 1)
    | 'n' ->
        let n = param_at params 0 0 in
        if has '?' && n = 6 then "Request extended cursor position"
        else if prefix = "" && n = 6 then "Request cursor position"
        else if prefix = "" && n = 5 then "Request device status"
        else "Unknown"
    | 's' when prefix = "" -> "Save cursor position"
    | 'r' when prefix = "" ->
        Fmt.str "Set scrolling region top=%d bottom=%d" (positive params 0 1)
          (param_at params 1 0)
    | 'J' -> describe_erase_display ~selective:(has '?') (param_at params 0 0)
    | 'K' -> describe_erase_line ~selective:(has '?') (param_at params 0 0)
    | 'L' -> Fmt.str "Insert %d blank line(s)" (positive params 0 1)
    | 'M' -> Fmt.str "Delete %d line(s)" (positive params 0 1)
    | 'S' -> Fmt.str "Scroll up %d line(s)" (positive params 0 1)
    | 'T' -> Fmt.str "Scroll down %d line(s)" (positive params 0 1)
    | 'p' when has_inter '$' ->
        describe_mode ~action:Mode_request ~private_:(has '?') (param_at params 0 0)
    | 'h' -> describe_mode ~action:Mode_enable ~private_:(has '?') (param_at params 0 0)
    | 'l' -> describe_mode ~action:Mode_disable ~private_:(has '?') (param_at params 0 0)
    | _ -> "Unknown"

let describe_esc ~intermediates ~final =
  if intermediates <> "" then "Unknown"
  else
    match final with
    | '7' -> "Save cursor"
    | '8' -> "Restore cursor"
    | '>' -> "Normal keypad"
    | '=' -> "Application keypad"
    | '\\' -> "String terminator"
    | _ -> "Unknown"

let hex_digit = function
  | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' as c -> Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' as c -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> None

let hex_decode s =
  let len = String.length s in
  if len = 0 || len mod 2 <> 0 then None
  else
    let buf = Buffer.create (len / 2) in
    let rec loop i =
      if i >= len then Some (Buffer.contents buf)
      else
        match (hex_digit s.[i], hex_digit s.[i + 1]) with
        | Some hi, Some lo ->
            Buffer.add_char buf (Char.chr ((hi * 16) + lo));
            loop (i + 2)
        | _ -> None
    in
    loop 0

(* XTGETTCAP (DCS + q ...): each ';'-separated field is the hex encoding of a termcap or
   terminfo capability name. A field that fails to decode as hex is shown as received. *)
let describe_termcap data =
  if data = "" then "Unknown"
  else
    let names = String.split_on_char ';' data in
    let decoded =
      List.map (fun part -> match hex_decode part with Some s -> s | None -> part) names
    in
    Fmt.str "Request termcap entry for %s" (String.concat ", " decoded)

let describe_dcs ~intermediates ~final ~data =
  let prefix, inter = split_intermediates intermediates in
  if String.contains inter '+' && final = 'q' then describe_termcap data
  else if String.contains prefix '>' && final = '|' then
    Fmt.str "Terminal name and version: %s" data
  else "Unknown"

let dcs_body ~params ~intermediates ~final ~data =
  Fmt.str "%s %S" (csi_body ~params ~intermediates ~final) data

let osc_body fields = String.concat ";" fields

(* The parser splits an OSC payload on every ';', including ones inside free-form text
   such as a title or a URL; joining the trailing fields back together recovers that
   text. *)
let join_from n fields =
  let rec drop i = function
    | [] -> []
    | _ :: tl when i < n -> drop (i + 1) tl
    | rest -> rest
  in
  String.concat ";" (drop 0 fields)

let describe_osc fields =
  match fields with
  | [] -> "Unknown"
  | cmd :: _ -> (
      match int_of_string_opt cmd with
      | Some 0 -> Fmt.str "Set icon name and window title to %S" (join_from 1 fields)
      | Some 1 -> Fmt.str "Set icon name to %S" (join_from 1 fields)
      | Some 2 -> Fmt.str "Set window title to %S" (join_from 1 fields)
      | Some 8 -> (
          match fields with
          | _ :: params_field :: _ :: _ ->
              let url = join_from 2 fields in
              if url = "" then "Close hyperlink"
              else if params_field = "" then Fmt.str "Set hyperlink to %S" url
              else
                Fmt.str "Set hyperlink to %S (%s)" url
                  (String.concat ", " (String.split_on_char ':' params_field))
          | _ -> "Unknown")
      | Some 9 -> Fmt.str "Show desktop notification %S" (join_from 1 fields)
      | Some 52 -> (
          match fields with
          | _ :: target :: _ ->
              let clip =
                match target with "c" -> "system" | "p" -> "primary" | _ -> target
              in
              let data = join_from 2 fields in
              if data = "?" then Fmt.str "Request %s clipboard" clip
              else Fmt.str "Set %s clipboard to base64 %S" clip data
          | _ -> "Unknown")
      | Some 10 when join_from 1 fields = "?" -> "Request foreground color"
      | Some 11 when join_from 1 fields = "?" -> "Request background color"
      | Some 12 when join_from 1 fields = "?" -> "Request cursor color"
      | _ -> "Unknown")

let line_of_action = function
  | Parser.Print s -> Fmt.str "Print %S" s
  | Parser.Execute c -> Fmt.str "Execute %s" (ctrl_name (Char.code c))
  | Parser.Csi { params; intermediates; final } ->
      Fmt.str "CSI %s  %s"
        (csi_body ~params ~intermediates ~final)
        (describe_csi ~params ~intermediates ~final)
  | Parser.Esc { intermediates; final } ->
      Fmt.str "ESC %s  %s"
        (csi_body ~params:[] ~intermediates ~final)
        (describe_esc ~intermediates ~final)
  | Parser.Osc fields -> Fmt.str "OSC %s  %s" (osc_body fields) (describe_osc fields)
  | Parser.Dcs { params; intermediates; final; data } ->
      Fmt.str "DCS %s  %s"
        (dcs_body ~params ~intermediates ~final ~data)
        (describe_dcs ~intermediates ~final ~data)
  | Parser.Apc s -> Fmt.str "APC %s" (quote s)
  | Parser.Pm s -> Fmt.str "PM %s  Privacy message" (quote s)
  | Parser.Sos s -> Fmt.str "SOS %s  Control string" (quote s)

let explain input =
  let p = Parser.create () in
  (* Sequenced with distinct [let]s: OCaml does not specify the evaluation order of
     the two operands of [(@)], and evaluating [flush] before [feed] would silently
     drop actions completed only by the trailing bytes. *)
  let fed = Parser.feed p input in
  let flushed = Parser.flush p in
  let actions = fed @ flushed in
  let buf = Buffer.create (String.length input * 2) in
  let pending = Buffer.create 16 in
  let flush_pending () =
    if Buffer.length pending > 0 then begin
      Buffer.add_string buf (Fmt.str "Print %S" (Buffer.contents pending));
      Buffer.add_char buf '\n';
      Buffer.clear pending
    end
  in
  List.iter
    (fun action ->
      match action with
      | Parser.Print s -> Buffer.add_string pending s
      | _ ->
          flush_pending ();
          Buffer.add_string buf (line_of_action action);
          Buffer.add_char buf '\n')
    actions;
  flush_pending ();
  Buffer.contents buf
