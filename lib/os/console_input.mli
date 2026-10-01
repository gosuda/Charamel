(** Win32 console input records decoded to bytes.

    A POSIX console delivers the bytes a terminal produces; a Windows console delivers
    structured [INPUT_RECORD]s, so on Windows the input path has to turn those records
    back into the VT byte stream the rest of the program already parses. This module owns
    that translation, and nothing about it is Windows-only at test time: {!val:encode} is
    a pure function over {!type:input_record} values, unit-tested on every platform, while
    {!val:records} is the reader that only produces anything on Windows.

    The bytes {!val:encode} emits are the bytes a VT500-class terminal would have sent for
    the same key press, in the forms [Charamel_tea.Input] decodes: [CSI] letter and
    [CSI n ; m] sequences for the special keys, UTF-8 for text, [SS3] for the first four
    function keys, and the [DEC] reports for size and focus. Feeding the result to
    [Charamel_tea.Input.feed] therefore needs no Windows-specific decoder. *)

(** One decoded console input record: a key press or release, a buffer resize, or a focus
    change. Values of this type are data, so a test can synthesize them; on Windows
    {!val:records} produces them from [ReadConsoleInputW]. [Ignored] stands for every
    record no Charamel application can observe, notably [MOUSE_EVENT] and [MENU_EVENT],
    which the Windows reader drops rather than interprets. *)
type input_record = Os_platform.input_record =
  | Key_event of key_event
  | Buffer_size of { rows : int; cols : int }
  | Focus of bool
  | Ignored

and key_event = Os_platform.key_event = {
  down : bool;
  repeat : int;
  virtual_key : int;
  wide_char : int;
  control_key_state : int;
}
(** The fields of a [Key_event] record: [down] distinguishes press from release, [repeat]
    is the Win32 [wRepeatCount], [virtual_key] is a [VK_*] code, [wide_char] is one UTF-16
    code unit and is [0] when the record carries no character, and [control_key_state]
    carries the [SHIFT_PRESSED], [LEFT_CTRL_PRESSED], [RIGHT_CTRL_PRESSED],
    [LEFT_ALT_PRESSED] and [RIGHT_ALT_PRESSED] bits. *)

val encode : input_record list -> bytes
(** [encode records] is the VT byte stream those records stand for.

    Text: consecutive key-down units are accumulated as UTF-16 and emitted as UTF-8. A
    high surrogate followed by a low surrogate becomes one scalar, so a character above
    the basic plane survives being split across two records; a high surrogate at the end
    of the batch is dropped rather than emitted as a lone surrogate, and the next
    {!val:encode} call cannot remember it. Each completed character is emitted [repeat]
    times. Key-release records contribute nothing, because a terminal reports a release
    only when the application asks, and [Charamel_tea] does not.

    Special keys: a key-down with no character and a known [VK_*] code emits the bytes a
    VT terminal sends for that key — arrows, [HOME], [END], [PGUP], [PGDN], [INSERT],
    [DELETE], backspace, tab, enter, escape, space and [F1] to [F12] — parameterised as an
    escape, CSI, {e 1 ; m}, and the final letter when a modifier is held, with {e m} equal
    to {e 1 + shift + 2 * alt + 4 * ctrl}; a key whose unmodified form is a numbered
    escape sequence gains the same parameter in place of the trailing tilde. An unknown
    [VK_*] code, or a code held only as a modifier press, emits nothing. The count is
    honored the same way as for text.

    Structure: a [Buffer_size] record becomes the xtgetwinops text-area report — an
    escape, CSI, {e 8 ; rows ; cols t} — which is how a VT terminal answers a size query
    and so is what a reader needs to learn the size from the byte stream; a [Focus] record
    becomes an escape, CSI and {e I} when the window gained focus or {e O} when it lost
    it. [Ignored] records contribute nothing. *)

type console_input
(** A source of decoded input bytes: what a terminal program reads.

    The abstraction exists because the thing behind "keyboard input" differs per platform
    and per transport, while everything above it wants one operation — {!val:read}. A
    POSIX local terminal is a channel; a Windows local terminal is the record queue below,
    decoded by {!val:encode}; a remote session is a callback owned by the SSH layer; a
    scripted test is a queue of literals; a program that renders without reading is a
    source that never answers.

    Abstract: build one with {!val:of_channel}, {!val:of_console_records},
    {!val:of_reader}, {!val:of_queue} or {!val:blocked}. *)

val of_channel : Lwt_io.input_channel -> console_input
(** [of_channel channel] reads bytes from [channel] — the local POSIX terminal, from
    {!val:Charamel_os.Tty.open_controlling_in} or from standard input. A single
    {!val:read} returns what has arrived so far rather than waiting for a whole line,
    because a keypress is not a line; it returns [[]] once the channel is closed. On
    Windows a channel is the wrong shape for console input — reading [CONIN$] as bytes
    bypasses the record queue — so this constructor is for pipes, files and sockets there.
*)

val of_console_records : unit -> console_input
(** [of_console_records ()] reads the Windows console: each {!val:read} waits for queued
    {!type:input_record}s and hands back their {!val:encode}d bytes, which is how the
    arrow-key and surrogate-pair handling reaches a decoder that only speaks VT. POSIX has
    no record queue, so a {!val:read} on this source answers [[]] — end of input — at
    once. *)

val of_reader : (unit -> string option Lwt.t) -> console_input
(** [of_reader reader] wraps a pump owned elsewhere: an SSH channel's agent requests, a
    pipe another component drains, a device file. [reader] answers [Some bytes] when there
    is something to deliver and [None] once the source is finished, which {!val:read}
    reports as [[]]; the callback is called again only after the previous call resolves,
    so a source never sees overlapping reads. *)

val of_queue : string list -> console_input
(** [of_queue chunks] is a scripted source delivering the chunks in order and then end of
    input. Nothing blocks: the list is the whole future of the source, which is what makes
    a key-by-key test independent of any scheduler. *)

val blocked : unit -> console_input
(** [blocked ()] never answers a {!val:read}. A program started with its renderer off, or
    one that has been given up to the operating system, still needs an input field, and
    this is the source that makes it read nothing forever rather than inventing an end of
    input that a caller would treat as a disconnect. *)

val read : console_input -> string Lwt.t
(** [read source] is the next bytes of input, or [[]] at end of input. It never raises for
    EOF: an SSH transport and a local terminal both signal a closed input by answering
    [[]], and a caller that has to distinguish the two distinguishes them at the source,
    not here. A read on a source that is still waiting simply waits. *)

val records : unit -> input_record list Lwt.t
(** [records ()] returns the console input records currently queued, in arrival order,
    after waiting for at least one. Windows counts the queued events with
    [GetNumberOfConsoleInputEvents] and consumes them with [ReadConsoleInputW] inside
    [Lwt_preemptive.detach], sleeping 10 ms between looks so that no pool thread is held
    while the user is not typing, exactly as the [10 ms] cadence upstream uses. Mouse and
    menu records arrive as [Ignored], so a caller sees that something happened without
    this module deciding what it meant.

    POSIX has no console records: the result is always [[]] and the call yields once
    without blocking, which lets a program that checks [Sys.win32] at runtime call this
    without special-casing the loop. *)
