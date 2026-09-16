(** Incremental parser for DEC ANSI escape sequences.

    The parser recognises the control functions of the DEC VT500 state machine described
    at https://vt100.net/emu/dec_ansi_parser. It consumes one byte at a time across calls,
    so a sequence may arrive split over any number of reads. The parameter, prefix and
    payload conventions described with [feed] are part of the public contract consumed by
    the input decoder and the screen renderer. *)

type action =
  | Print of string
      (** One complete UTF-8 rune of printable text, held as its UTF-8 bytes. An invalid
          rune completes as U+FFFD. *)
  | Execute of char  (** A C0 control, a C1 control or DEL. *)
  | Csi of { params : int option list list; intermediates : string; final : char }
      (** A control sequence. [final] is the final byte. *)
  | Esc of { intermediates : string; final : char }
      (** An escape sequence, such as SS3 or the standalone two byte string terminator.
          The two byte [ESC] backslash terminator of an [Apc], [Pm] or [Sos] string is
          consumed by the string itself and reports no action; after [Osc] and [Dcs] it
          still reports here. *)
  | Osc of string list
      (** An operating system command. The payload is split on [';']. The first field is
          the command number when the payload begins with digits, and an empty payload
          yields a single empty field. *)
  | Dcs of {
      params : int option list list;
      intermediates : string;
      final : char;
      data : string;
    }
      (** A device control string. [data] holds the payload bytes verbatim. A string
          entered without a final byte, which happens when ESC itself is put into the
          payload, reports ['\000'] as [final]. *)
  | Apc of string  (** An application program command payload. *)
  | Pm of string  (** A privacy message payload. *)
  | Sos of string  (** A start of string payload. *)

type t
(** The type for parsers. A parser is stateful; use one parser per input stream. *)

val create : unit -> t
(** [create ()] is a parser in the ground state. *)

val feed : t -> string -> action list
(** [feed t s] consumes the bytes of [s] and returns the actions completed by those bytes,
    in order. Bytes that belong to an incomplete sequence are kept in [t], and the actions
    they later complete are returned by a subsequent [feed]. Feeding [s] in chunks returns
    the same actions as feeding [s] at once.

    In [Csi] and [Dcs], [params] lists the declared parameters in order. Each parameter is
    the list of its colon separated sub-values, and [None] marks a declared slot that
    holds no value, both for an empty parameter between semicolons and for an empty
    sub-value between colons. At most 32 slots are kept per sequence, colon sub-parameters
    included, and later slots are dropped.

    [intermediates] holds the private prefix bytes, one of [? < > =], in arrival order
    followed by the intermediate bytes, 0x20 to 0x2F. At most two of each are kept, so the
    string never exceeds four bytes, and the prefixes always precede the intermediates.
    The prefixes live in [intermediates] because the action type has no separate prefix
    field.

    A string payload is truncated at 65536 bytes. Further payload bytes are discarded
    while the sequence still terminates only at its normal terminator, so storage stays
    bounded through arbitrarily long payload input.

    A printable rune is reported by one [Print] action per rune. Bytes collected for an
    incomplete rune swallow every byte, controls included, until the length announced by
    the lead byte is reached, and an invalid rune completes as U+FFFD. In SOS, PM and APC
    a UTF-8 lead byte abandons the string, and the rune prints once it is complete. The
    two byte [ESC] backslash terminator ends an [Apc], [Pm] or [Sos] string silently,
    while after [Osc] and [Dcs] it completes as an [Esc] action. *)

val flush : t -> action list
(** [flush t] ends pending partial input and returns the actions it completes. A lone
    pending ESC becomes [Execute '\027'] and a partially collected rune becomes [Print] of
    its bytes. Every other partial sequence is cancelled without an action. [flush t]
    leaves the parser in the ground state. *)
