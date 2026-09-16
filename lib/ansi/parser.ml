type action =
  | Print of string
  | Execute of char
  | Csi of { params : int option list list; intermediates : string; final : char }
  | Esc of { intermediates : string; final : char }
  | Osc of string list
  | Dcs of {
      params : int option list list;
      intermediates : string;
      final : char;
      data : string;
    }
  | Apc of string
  | Pm of string
  | Sos of string

let max_params = 32
let max_prefixes = 2
let max_intermediates = 2
let max_data = 65536
let replacement = "\xef\xbf\xbd"

type state =
  | Ground
  | Csi_entry
  | Csi_intermediate
  | Csi_param
  | Dcs_entry
  | Dcs_intermediate
  | Dcs_param
  | Dcs_string
  | Escape
  | Escape_intermediate
  | String_escape
  | Osc_string
  | Sos_string
  | Pm_string
  | Apc_string
  | Utf8

type kind =
  | K_none
  | K_clear
  | K_collect
  | K_prefix
  | K_dispatch
  | K_execute
  | K_start
  | K_put
  | K_param
  | K_print

(* The transition function mirrors the DEC VT500 table of
   .references/x/ansi/parser/transition_table.go, including its deviations:
   ':' builds sub-parameters, OSC and DCS payloads take bytes up to 0xFF,
   SOS/PM/APC dispatch, DEL is collected inside DCS, the C1 ST (0x9C)
   terminates, and a SOS, PM or APC string consumes its two-byte ESC \
   terminator silently per ECMA-48 instead of leaving it to the Escape state.
   Per-state entries override the anywhere rules, and within the anywhere
   rules the C1 DCS introducer 0x90 is checked before the C1 execute range,
   matching the reference table's final overwrite. *)
let anywhere b =
  if b = 0x18 || b = 0x1a || b = 0x99 || b = 0x9a then (K_execute, Ground)
  else if b = 0x90 then (K_clear, Dcs_entry)
  else if (b >= 0x80 && b <= 0x8f) || (b >= 0x91 && b <= 0x97) || b = 0x9c then
    (K_execute, Ground)
  else if b = 0x1b then (K_clear, Escape)
  else if b = 0x98 then (K_start, Sos_string)
  else if b = 0x9e then (K_start, Pm_string)
  else if b = 0x9f then (K_start, Apc_string)
  else if b = 0x9b then (K_clear, Csi_entry)
  else if b = 0x9d then (K_start, Osc_string)
  else if (b >= 0xc2 && b <= 0xdf) || (b >= 0xe0 && b <= 0xef) || (b >= 0xf0 && b <= 0xf4)
  then (K_collect, Utf8)
  else (K_none, Ground)

(* [Escape] and [String_escape] share every transition except the backslash:
   from [Escape] it dispatches the standalone two-byte string terminator, while
   from [String_escape] it silently completes the terminator that ended the
   preceding SOS, PM or APC string, per ECMA-48. *)
let escape_transition st b =
  if b <= 0x17 || b = 0x19 || (b >= 0x1c && b <= 0x1f) then (K_execute, st)
  else if b = 0x7f then (K_none, st)
  else if
    (b >= 0x30 && b <= 0x4f)
    || (b >= 0x51 && b <= 0x57)
    || b = 0x59 || b = 0x5a || b = 0x5c
    || (b >= 0x60 && b <= 0x7e)
  then (K_dispatch, Ground)
  else if b >= 0x20 && b <= 0x2f then (K_collect, Escape_intermediate)
  else
    match b with
    | 0x50 -> (K_clear, Dcs_entry)
    | 0x5b -> (K_clear, Csi_entry)
    | 0x58 -> (K_start, Sos_string)
    | 0x5d -> (K_start, Osc_string)
    | 0x5e -> (K_start, Pm_string)
    | 0x5f -> (K_start, Apc_string)
    | _ -> anywhere b

let transition st b =
  match st with
  | Ground ->
      if b <= 0x17 || b = 0x19 || (b >= 0x1c && b <= 0x1f) || b = 0x7f then
        (K_execute, Ground)
      else if b >= 0x20 && b <= 0x7e then (K_print, Ground)
      else anywhere b
  | Escape -> escape_transition Escape b
  | String_escape ->
      (* ECMA-48: the backslash silently completes the ESC \ terminator that
         ended the preceding SOS, PM or APC string; the string action itself
         dispatched at the ESC. *)
      if b = 0x5c then (K_none, Ground) else escape_transition String_escape b
  | Escape_intermediate ->
      if b <= 0x17 || b = 0x19 || (b >= 0x1c && b <= 0x1f) then
        (K_execute, Escape_intermediate)
      else if b >= 0x20 && b <= 0x2f then (K_collect, Escape_intermediate)
      else if b = 0x7f then (K_none, Escape_intermediate)
      else if b >= 0x30 && b <= 0x7e then (K_dispatch, Ground)
      else anywhere b
  | Csi_entry ->
      if b <= 0x17 || b = 0x19 || (b >= 0x1c && b <= 0x1f) then (K_execute, Csi_entry)
      else if b = 0x7f then (K_none, Csi_entry)
      else if b >= 0x40 && b <= 0x7e then (K_dispatch, Ground)
      else if b >= 0x20 && b <= 0x2f then (K_collect, Csi_intermediate)
      else if b >= 0x30 && b <= 0x3b then (K_param, Csi_param)
      else if b >= 0x3c && b <= 0x3f then (K_prefix, Csi_param)
      else anywhere b
  | Csi_param ->
      if b <= 0x17 || b = 0x19 || (b >= 0x1c && b <= 0x1f) then (K_execute, Csi_param)
      else if b >= 0x30 && b <= 0x3b then (K_param, Csi_param)
      else if b = 0x7f || (b >= 0x3c && b <= 0x3f) then (K_none, Csi_param)
      else if b >= 0x40 && b <= 0x7e then (K_dispatch, Ground)
      else if b >= 0x20 && b <= 0x2f then (K_collect, Csi_intermediate)
      else anywhere b
  | Csi_intermediate ->
      if b <= 0x17 || b = 0x19 || (b >= 0x1c && b <= 0x1f) then
        (K_execute, Csi_intermediate)
      else if b >= 0x20 && b <= 0x2f then (K_collect, Csi_intermediate)
      else if b = 0x7f then (K_none, Csi_intermediate)
      else if b >= 0x40 && b <= 0x7e then (K_dispatch, Ground)
      else if b >= 0x30 && b <= 0x3f then (K_none, Ground)
      else anywhere b
  | Dcs_entry ->
      if
        (b >= 0x00 && b <= 0x07)
        || (b >= 0x0e && b <= 0x17)
        || b = 0x19
        || (b >= 0x1c && b <= 0x1f)
        || b = 0x7f
      then (K_none, Dcs_entry)
      else if b >= 0x20 && b <= 0x2f then (K_collect, Dcs_intermediate)
      else if b >= 0x30 && b <= 0x3b then (K_param, Dcs_param)
      else if b >= 0x3c && b <= 0x3f then (K_prefix, Dcs_param)
      else if (b >= 0x08 && b <= 0x0d) || b = 0x1b then (K_put, Dcs_string)
      else if b >= 0x40 && b <= 0x7e then (K_start, Dcs_string)
      else anywhere b
  | Dcs_intermediate ->
      if b <= 0x17 || b = 0x19 || (b >= 0x1c && b <= 0x1f) || b = 0x7f then
        (K_none, Dcs_intermediate)
      else if b >= 0x20 && b <= 0x2f then (K_collect, Dcs_intermediate)
      else if (b >= 0x30 && b <= 0x3f) || (b >= 0x40 && b <= 0x7e) then
        (K_start, Dcs_string)
      else anywhere b
  | Dcs_param ->
      if b <= 0x17 || b = 0x19 || (b >= 0x1c && b <= 0x1f) || b = 0x7f then
        (K_none, Dcs_param)
      else if b >= 0x30 && b <= 0x3b then (K_param, Dcs_param)
      else if b >= 0x3c && b <= 0x3f then (K_none, Dcs_param)
      else if b >= 0x20 && b <= 0x2f then (K_collect, Dcs_intermediate)
      else if b >= 0x40 && b <= 0x7e then (K_start, Dcs_string)
      else anywhere b
  | Dcs_string ->
      if b = 0x1b then (K_dispatch, Escape)
      else if b = 0x9c then (K_dispatch, Ground)
      else if b = 0x18 || b = 0x1a then (K_none, Ground)
      else if b <= 0x17 || b = 0x19 || (b >= 0x1c && b <= 0xff) then (K_put, Dcs_string)
      else anywhere b
  | Osc_string ->
      if b = 0x1b then (K_dispatch, Escape)
      else if b = 0x07 || b = 0x9c then (K_dispatch, Ground)
      else if b = 0x18 || b = 0x1a then (K_none, Ground)
      else if
        (b >= 0x00 && b <= 0x06)
        || (b >= 0x08 && b <= 0x17)
        || b = 0x19
        || (b >= 0x1c && b <= 0x1f)
      then (K_none, Osc_string)
      else if b >= 0x20 && b <= 0xff then (K_put, Osc_string)
      else anywhere b
  | (Sos_string | Pm_string | Apc_string) as string_state ->
      if b = 0x1b then (K_dispatch, String_escape)
      else if b = 0x9c then (K_dispatch, Ground)
      else if b = 0x18 || b = 0x1a then (K_none, Ground)
      else if b <= 0x17 || b = 0x19 || (b >= 0x1c && b <= 0x7f) then (K_put, string_state)
      else anywhere b
  | Utf8 -> anywhere b

type t = {
  mutable st : state;
  mutable count : int;
  vals : int array;
  flags : int array;
  inter : Bytes.t;
  mutable ninter : int;
  mutable nprefix : int;
  mutable final : int;
  data : Bytes.t;
  mutable ndata : int;
  rune : Bytes.t;
  mutable rlen : int;
}

let create () =
  {
    st = Ground;
    count = 0;
    vals = Array.make max_params 0;
    flags = Array.make max_params 0;
    inter = Bytes.create (max_prefixes + max_intermediates);
    ninter = 0;
    nprefix = 0;
    final = 0;
    data = Bytes.create max_data;
    ndata = 0;
    rune = Bytes.create 4;
    rlen = 0;
  }

let has_digits t i = t.flags.(i) land 2 <> 0
let has_more t i = t.flags.(i) land 1 <> 0

(* Upstream clears the packed command when the parser leaves EscapeState so
   a sequence following a string terminator starts clean; per-sequence
   fields here are reset at every sequence entry, and this clear keeps the
   dispatch after an OSC or DCS string terminator fresh as well. *)
let clear_seq t =
  t.count <- 0;
  t.flags.(0) <- 0;
  t.ninter <- 0;
  t.nprefix <- 0;
  t.final <- 0

let store_prefix t b =
  if t.nprefix < max_prefixes then begin
    Bytes.set t.inter t.ninter (Char.chr b);
    t.ninter <- t.ninter + 1;
    t.nprefix <- t.nprefix + 1
  end

let store_intermediate t b =
  if t.ninter - t.nprefix < max_intermediates then begin
    Bytes.set t.inter t.ninter (Char.chr b);
    t.ninter <- t.ninter + 1
  end

let put_data t b =
  if t.ndata < max_data then begin
    Bytes.set t.data t.ndata (Char.chr b);
    t.ndata <- t.ndata + 1
  end

let data_string t = Bytes.sub_string t.data 0 t.ndata
let inter_string t = Bytes.sub_string t.inter 0 t.ninter

(* A trailing value slot counts at dispatch time; slots beyond the 32
   budget are never declared, so the count never exceeds it. *)
let bump_count t =
  if (t.count > 0 && t.count < max_params) || (t.count = 0 && has_digits t 0) then
    t.count <- t.count + 1

let build_params t =
  let rec group i current groups =
    if i >= t.count then
      match current with
      | [] -> List.rev groups
      | current -> List.rev (List.rev current :: groups)
    else
      let v = if has_digits t i then Some t.vals.(i) else None in
      if has_more t i then group (i + 1) (v :: current) groups
      else group (i + 1) [] (List.rev (v :: current) :: groups)
  in
  group 0 [] []

let utf8_lead_len lead =
  if lead >= 0xc2 && lead <= 0xdf then 2
  else if lead >= 0xe0 && lead <= 0xef then 3
  else if lead >= 0xf0 && lead <= 0xf4 then 4
  else 1

let valid_rune rune len =
  let get i = Char.code (Bytes.get rune i) in
  let cont i =
    let c = get i in
    c >= 0x80 && c <= 0xbf
  in
  match len with
  | 2 -> cont 1
  | 3 ->
      let lead = get 0 in
      (cont 1 && cont 2)
      && not ((lead = 0xe0 && get 1 < 0xa0) || (lead = 0xed && get 1 > 0x9f))
  | 4 ->
      let lead = get 0 in
      (cont 1 && cont 2 && cont 3)
      && not ((lead = 0xf0 && get 1 < 0x90) || (lead = 0xf4 && get 1 > 0x8f))
  | _ -> true

let perform t out kind b next =
  match kind with
  | K_none -> ()
  | K_clear -> clear_seq t
  | K_prefix -> store_prefix t b
  | K_collect ->
      if next = Utf8 then begin
        t.rlen <- 0;
        Bytes.set t.rune 0 (Char.chr b);
        t.rlen <- 1
      end
      else store_intermediate t b
  | K_execute -> out := Execute (Char.chr b) :: !out
  | K_print -> out := Print (String.make 1 (Char.chr b)) :: !out
  | K_param ->
      if t.count < max_params then begin
        if b >= 0x30 && b <= 0x39 then begin
          if not (has_digits t t.count) then begin
            t.vals.(t.count) <- 0;
            t.flags.(t.count) <- t.flags.(t.count) lor 2
          end;
          t.vals.(t.count) <- (t.vals.(t.count) * 10) + (b - 0x30)
        end;
        if b = 0x3a then t.flags.(t.count) <- t.flags.(t.count) lor 1;
        if b = 0x3b || b = 0x3a then begin
          t.count <- t.count + 1;
          if t.count < max_params then t.flags.(t.count) <- 0
        end
      end
  | K_start ->
      t.ndata <- 0;
      if next = Dcs_string then t.final <- b
  | K_put -> put_data t b
  | K_dispatch -> (
      bump_count t;
      match t.st with
      | Csi_entry | Csi_param | Csi_intermediate ->
          t.final <- b;
          out :=
            Csi
              {
                params = build_params t;
                intermediates = inter_string t;
                final = Char.chr b;
              }
            :: !out
      | Escape | Escape_intermediate | String_escape ->
          t.final <- b;
          out := Esc { intermediates = inter_string t; final = Char.chr b } :: !out
      | Dcs_entry | Dcs_param | Dcs_intermediate | Dcs_string ->
          out :=
            Dcs
              {
                params = build_params t;
                intermediates = inter_string t;
                final = Char.chr t.final;
                data = data_string t;
              }
            :: !out
      | Osc_string -> out := Osc (String.split_on_char ';' (data_string t)) :: !out
      | Sos_string -> out := Sos (data_string t) :: !out
      | Pm_string -> out := Pm (data_string t) :: !out
      | Apc_string -> out := Apc (data_string t) :: !out
      | Ground | Utf8 -> ())

let step t out b =
  if t.st = Utf8 then begin
    if t.rlen < 4 then begin
      Bytes.set t.rune t.rlen (Char.chr b);
      t.rlen <- t.rlen + 1
    end;
    let rw = utf8_lead_len (Char.code (Bytes.get t.rune 0)) in
    if t.rlen >= rw then begin
      let s = Bytes.sub_string t.rune 0 rw in
      out := Print (if valid_rune t.rune rw then s else replacement) :: !out;
      t.st <- Ground;
      t.rlen <- 0
    end
  end
  else begin
    let kind, next = transition t.st b in
    if (t.st = Escape || t.st = String_escape) && next <> Escape && next <> String_escape
    then clear_seq t;
    (* Upstream starts the DCS payload with a zero command byte when the
       entry into the string happens through a put byte, including ESC. *)
    if kind = K_put && t.st = Dcs_entry && next = Dcs_string then begin
      t.final <- 0;
      t.ndata <- 0
    end;
    if b = 0x1b && (t.st = Escape || t.st = String_escape) then
      out := Execute '\027' :: !out
    else perform t out kind b next;
    t.st <- next
  end

let feed t s =
  let out = ref [] in
  for i = 0 to String.length s - 1 do
    step t out (String.get_uint8 s i)
  done;
  List.rev !out

let flush t =
  match t.st with
  | Escape | String_escape ->
      t.st <- Ground;
      clear_seq t;
      [ Execute '\027' ]
  | Escape_intermediate ->
      t.st <- Ground;
      clear_seq t;
      []
  | Utf8 ->
      let s = Bytes.sub_string t.rune 0 t.rlen in
      t.st <- Ground;
      t.rlen <- 0;
      [ Print s ]
  | Ground -> []
  | Csi_entry | Csi_intermediate | Csi_param | Dcs_entry | Dcs_intermediate | Dcs_param
  | Dcs_string | Osc_string | Sos_string | Pm_string | Apc_string ->
      t.st <- Ground;
      clear_seq t;
      t.ndata <- 0;
      []
