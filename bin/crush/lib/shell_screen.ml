open Result.Syntax

type dialect = Posix | Cmd
type verdict = Clear | Blocked of string | Review of string

let platform = if Sys.win32 then Cmd else Posix
let max_depth = 8

let refused_programs =
  [
    "alias";
    "aria2c";
    "axel";
    "chrome";
    "curl";
    "curlie";
    "firefox";
    "httpie";
    "http-prompt";
    "links";
    "lynx";
    "nc";
    "safari";
    "scp";
    "ssh";
    "telnet";
    "w3m";
    "wget";
    "xh";
    "doas";
    "su";
    "sudo";
    "apk";
    "apt";
    "apt-cache";
    "apt-get";
    "dnf";
    "dpkg";
    "emerge";
    "home-manager";
    "makepkg";
    "opkg";
    "pacman";
    "paru";
    "pkg";
    "pkg_add";
    "pkg_delete";
    "portage";
    "rpm";
    "yay";
    "yum";
    "zypper";
    "at";
    "batch";
    "chkconfig";
    "crontab";
    "fdisk";
    "mkfs";
    "mount";
    "parted";
    "service";
    "systemctl";
    "umount";
    "firewall-cmd";
    "ifconfig";
    "ip";
    "iptables";
    "netstat";
    "pfctl";
    "route";
    "ufw";
    "kill";
    "pkill";
    "killall";
    "shutdown";
    "reboot";
    "poweroff";
    "halt";
    "eval";
    "exec";
    "source";
  ]

let refused_sequences =
  [
    [ "apk"; "add" ];
    [ "apt"; "install" ];
    [ "apt-get"; "install" ];
    [ "dnf"; "install" ];
    [ "pacman"; "-s" ];
    [ "pkg"; "install" ];
    [ "yum"; "install" ];
    [ "zypper"; "install" ];
    [ "brew"; "install" ];
    [ "cargo"; "install" ];
    [ "gem"; "install" ];
    [ "go"; "install" ];
    [ "npm"; "install"; "--global" ];
    [ "npm"; "install"; "-g" ];
    [ "pip"; "install"; "--user" ];
    [ "pip3"; "install"; "--user" ];
    [ "pnpm"; "add"; "--global" ];
    [ "pnpm"; "add"; "-g" ];
    [ "yarn"; "global"; "add" ];
    [ "go"; "test"; "-exec" ];
  ]

let posix_shells =
  [
    "sh";
    "bash";
    "rbash";
    "dash";
    "zsh";
    "ksh";
    "ksh93";
    "mksh";
    "pdksh";
    "ash";
    "yash";
    "posh";
  ]

let foreign_shells = [ "fish"; "csh"; "tcsh" ]
let code_runners = [ "."; "eval"; "source" ]

let wrappers =
  [
    "command";
    "builtin";
    "exec";
    "time";
    "coproc";
    "hash";
    "enable";
    "env";
    "nohup";
    "nice";
    "timeout";
    "stdbuf";
    "setsid";
    "busybox";
    "ionice";
    "chrt";
    "taskset";
    "unbuffer";
    "chroot";
    "flock";
    "runuser";
    "script";
    "doas";
    "sudo";
    "su";
  ]

let exec_flags =
  [
    ("find", [ "-exec"; "-execdir"; "-ok"; "-okdir" ]);
    ("fd", [ "-x"; "-X"; "--exec"; "--exec-batch" ]);
  ]

let code_flags =
  [
    ("env", [ "-S"; "--split-string" ]);
    ("flock", [ "-c"; "--command" ]);
    ("runuser", [ "-c"; "--command" ]);
    ("script", [ "-c"; "--command" ]);
  ]

let cmd_runners = [ "if"; "for"; "do"; "else"; "call"; "start"; "cmd" ]
let executable_suffixes = [ ".exe"; ".com"; ".bat"; ".cmd" ]

let reserved_starters =
  [ "!"; "{"; "}"; "if"; "then"; "else"; "elif"; "fi"; "do"; "done"; "while"; "until" ]

let computed_program = "the shell computes the name of the program it runs"
let sourced_code = "the command runs shell code the screen cannot read"
let stdin_code = "a shell reads its code from standard input"
let computed_code = "a shell runs a computed command string"
let foreign_code = "the screen cannot read code written for this shell"
let too_deep = "commands nest too deeply to screen"
let unreadable message = "the command cannot be parsed: " ^ message
let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'
let is_name_start c = is_letter c || c = '_'
let is_name_char c = is_name_start c || is_digit c
let is_all_digits text = text <> "" && String.for_all is_digit text
let suffix_from text start = String.sub text start (String.length text - start)

let contains_substring ~needle haystack =
  let width = String.length needle in
  let limit = String.length haystack - width in
  let rec at index =
    index <= limit
    && (String.equal (String.sub haystack index width) needle || at (index + 1))
  in
  width > 0 && at 0

let is_fragment_char dialect c =
  is_letter c || is_digit c
  ||
  match c with
  | '_' | '-' | '.' | '/' | ':' -> true
  | '=' -> ( match dialect with Posix -> true | Cmd -> false)
  | _ -> false

let fragments dialect text =
  let words = ref [] in
  let current = Buffer.create 16 in
  let flush () =
    if Buffer.length current > 0 then (
      words := Buffer.contents current :: !words;
      Buffer.clear current)
  in
  String.iter
    (fun c ->
      if is_fragment_char dialect c then Buffer.add_char current (Char.lowercase_ascii c)
      else flush ())
    text;
  flush ();
  List.rev !words

let drop_leading c text =
  let length = String.length text in
  let rec first index =
    if index < length && text.[index] = c then first (index + 1) else index
  in
  suffix_from text (first 0)

let basename text =
  let name = String.lowercase_ascii text in
  let start =
    match (String.rindex_opt name '/', String.rindex_opt name '\\') with
    | Some slash, Some backslash -> max slash backslash + 1
    | Some index, None | None, Some index -> index + 1
    | None, None -> 0
  in
  drop_leading '=' (suffix_from name start)

let strip_executable_suffix base =
  match
    List.find_opt
      (fun suffix ->
        String.length base > String.length suffix && String.ends_with ~suffix base)
      executable_suffixes
  with
  | Some suffix -> String.sub base 0 (String.length base - String.length suffix)
  | None -> base

let rec has_sequence name_of wanted names =
  match (wanted, names) with
  | [], _ -> true
  | _ :: _, [] -> false
  | first :: rest, name :: tail ->
      if String.equal (name_of name) first then has_sequence name_of rest tail
      else has_sequence name_of wanted tail

let refused names =
  match List.find_opt (fun name -> List.mem name refused_programs) names with
  | Some name -> Some name
  | None ->
      List.find_opt (fun sequence -> has_sequence Fun.id sequence names) refused_sequences
      |> Option.map (String.concat " ")

type word = { value : string; fixed : bool; unquoted : bool; assignment : bool }

type builder = {
  buf : Buffer.t;
  mutable is_fixed : bool;
  mutable is_unquoted : bool;
  mutable is_assignment : bool;
  mutable seen_equals : bool;
  mutable braces : int;
  mutable brace_list : bool;
  mutable bracket : bool;
}

let builder () =
  {
    buf = Buffer.create 16;
    is_fixed = true;
    is_unquoted = true;
    is_assignment = false;
    seen_equals = false;
    braces = 0;
    brace_list = false;
    bracket = false;
  }

let word_of b =
  {
    value = Buffer.contents b.buf;
    fixed = b.is_fixed;
    unquoted = b.is_unquoted;
    assignment = b.is_assignment;
  }

let is_name text =
  text <> "" && is_name_start text.[0] && String.for_all is_name_char text

let is_assignment_name name =
  if String.ends_with ~suffix:"+" name then
    is_name (String.sub name 0 (String.length name - 1))
  else is_name name

let is_brace_name text =
  let length = String.length text in
  length > 2
  && text.[0] = '{'
  && text.[length - 1] = '}'
  && is_name (String.sub text 1 (length - 2))

let follows_dot b =
  let length = Buffer.length b.buf in
  length > 0 && Buffer.nth b.buf (length - 1) = '.'

let add_unquoted b c =
  (match c with
  | '*' | '?' -> b.is_fixed <- false
  | '[' -> b.bracket <- true
  | ']' -> if b.bracket then b.is_fixed <- false
  | '{' -> b.braces <- b.braces + 1
  | ',' -> if b.braces > 0 then b.brace_list <- true
  | '.' -> if b.braces > 0 && follows_dot b then b.brace_list <- true
  | '}' ->
      if b.braces > 0 then (
        b.braces <- b.braces - 1;
        if b.brace_list then b.is_fixed <- false)
  | '=' ->
      if not b.seen_equals then (
        b.seen_equals <- true;
        b.is_assignment <- b.is_unquoted && is_assignment_name (Buffer.contents b.buf))
  | _ -> ());
  Buffer.add_char b.buf c

let add_quoted b c =
  Buffer.add_char b.buf c;
  b.is_unquoted <- false

let add_quoted_string b text =
  Buffer.add_string b.buf text;
  b.is_unquoted <- false

let mark_expansion b =
  Buffer.add_char b.buf '\000';
  b.is_fixed <- false;
  b.is_unquoted <- false

type sink = {
  mutable words : string list;
  mutable commands : (int * word list) list;
  mutable reviews : string list;
  mutable refused_executable : string option;
}

let review sink reason = sink.reviews <- reason :: sink.reviews

type heredoc = { delimiter : string; expands : bool; strip_tabs : bool }
type token = Word of word | Op of string | Redirect | End

type lexer = {
  source : string;
  mutable pos : int;
  depth : int;
  ansi_quotes : bool;
  sink : sink;
  mutable delimiter_next : bool option;
  mutable heredocs : heredoc list;
  mutable nesting : int;
}

let lexer sink ~depth ~ansi_quotes source =
  {
    source;
    pos = 0;
    depth;
    ansi_quotes;
    sink;
    delimiter_next = None;
    heredocs = [];
    nesting = 0;
  }

let max_nesting = 64

let nest lx expand =
  if lx.nesting >= max_nesting then Error "expansions nest too deeply"
  else (
    lx.nesting <- lx.nesting + 1;
    let result = expand () in
    lx.nesting <- lx.nesting - 1;
    result)

let peek lx offset =
  let index = lx.pos + offset in
  if index < String.length lx.source then Some lx.source.[index] else None

let advance lx count = lx.pos <- min (String.length lx.source) (lx.pos + count)

let is_metachar = function
  | ' ' | '\t' | '\n' | ';' | '&' | '|' | '(' | ')' | '<' | '>' -> true
  | _ -> false

let rec skip_blanks lx =
  match (peek lx 0, peek lx 1) with
  | Some (' ' | '\t'), _ ->
      advance lx 1;
      skip_blanks lx
  | Some '\\', Some '\n' ->
      advance lx 2;
      skip_blanks lx
  | _ -> ()

let rec skip_comment lx =
  match peek lx 0 with
  | None | Some '\n' -> ()
  | Some _ ->
      advance lx 1;
      skip_comment lx

let rec skip_name lx =
  match peek lx 0 with
  | Some c when is_name_char c ->
      advance lx 1;
      skip_name lx
  | Some _ | None -> ()

let operators =
  [
    "&&";
    "&";
    "||";
    "|";
    ";;&";
    ";;";
    ";&";
    ";";
    "(";
    ")";
    "<<<";
    "<<-";
    "<<";
    "<&";
    "<>";
    "<";
    ">>";
    ">&";
    ">|";
    ">";
  ]

let operator lx =
  let starts_here op =
    let length = String.length op in
    lx.pos + length <= String.length lx.source
    && String.equal (String.sub lx.source lx.pos length) op
  in
  let op =
    match List.find_opt starts_here operators with
    | Some op -> op
    | None -> String.sub lx.source lx.pos 1
  in
  advance lx (String.length op);
  match op with
  | "<<" | "<<-" ->
      lx.delimiter_next <- Some (String.equal op "<<-");
      Redirect
  | _ when op.[0] = '<' || op.[0] = '>' -> Redirect
  | _ -> Op op

let record_word lx w =
  lx.sink.words <- w.value :: lx.sink.words;
  match lx.delimiter_next with
  | None -> ()
  | Some strip_tabs ->
      lx.heredocs <-
        { delimiter = w.value; expands = w.unquoted; strip_tabs } :: lx.heredocs;
      lx.delimiter_next <- None

let trailing_backslashes line =
  let rec loop index count =
    if index < 0 then count
    else if line.[index] = '\\' then loop (index - 1) (count + 1)
    else count
  in
  loop (String.length line - 1) 0

let read_body lx doc =
  let length = String.length lx.source in
  let physical () =
    if lx.pos >= length then None
    else
      let stop =
        Option.value (String.index_from_opt lx.source lx.pos '\n') ~default:length
      in
      let line = String.sub lx.source lx.pos (stop - lx.pos) in
      lx.pos <- min length (stop + 1);
      Some (if doc.strip_tabs then drop_leading '\t' line else line)
  in
  (* An expanding here-document joins a line ending in an unescaped backslash
     with the next line before comparing against the delimiter, so the screen
     assembles the same logical lines the shell does. Only an odd trailing
     count continues the line; an escaped pair is a literal backslash. *)
  let rec lines acc pending =
    match physical () with
    | None ->
        review lx.sink (unreadable "unterminated here-document");
        List.rev acc
    | Some line when doc.expands && trailing_backslashes line mod 2 = 1 ->
        review lx.sink (unreadable "a here-document contains a line continuation");
        lines acc (pending ^ String.sub line 0 (String.length line - 1))
    | Some line ->
        let line = pending ^ line in
        if String.equal line doc.delimiter then List.rev acc else lines (line :: acc) ""
  in
  String.concat "\n" (lines [] "")

let digit_value ~base c =
  let value =
    match c with
    | '0' .. '9' -> Char.code c - Char.code '0'
    | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
    | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
    | _ -> base
  in
  if value < base then Some value else None

let read_number lx ~base ~limit =
  let rec loop value count =
    if count = limit then (value, count)
    else
      match Option.bind (peek lx 0) (digit_value ~base) with
      | Some digit ->
          advance lx 1;
          loop ((value * base) + digit) (count + 1)
      | None -> (value, count)
  in
  loop 0 0

let decode_escape lx b escape =
  match escape with
  | 'a' -> Buffer.add_char b.buf '\007'
  | 'b' -> Buffer.add_char b.buf '\b'
  | 'e' | 'E' -> Buffer.add_char b.buf '\027'
  | 'f' -> Buffer.add_char b.buf '\012'
  | 'n' -> Buffer.add_char b.buf '\n'
  | 'r' -> Buffer.add_char b.buf '\r'
  | 't' -> Buffer.add_char b.buf '\t'
  | 'v' -> Buffer.add_char b.buf '\011'
  | '\\' | '\'' | '"' | '?' -> Buffer.add_char b.buf escape
  | '0' .. '7' ->
      let rest, count = read_number lx ~base:8 ~limit:2 in
      let value = ((Char.code escape - Char.code '0') lsl (3 * count)) + rest in
      Buffer.add_char b.buf (Char.chr (value land 0xff))
  | 'x' -> (
      match read_number lx ~base:16 ~limit:2 with
      | _, 0 -> Buffer.add_string b.buf "\\x"
      | value, _ -> Buffer.add_char b.buf (Char.chr value))
  | 'u' | 'U' -> (
      let limit = if escape = 'u' then 4 else 8 in
      match read_number lx ~base:16 ~limit with
      | _, 0 ->
          Buffer.add_char b.buf '\\';
          Buffer.add_char b.buf escape
      | value, _ ->
          if Uchar.is_valid value then Buffer.add_utf_8_uchar b.buf (Uchar.of_int value))
  | 'c' -> (
      match peek lx 0 with
      | Some c ->
          advance lx 1;
          Buffer.add_char b.buf (Char.chr (Char.code c land 0x1f))
      | None -> Buffer.add_string b.buf "\\c")
  | _ ->
      Buffer.add_char b.buf '\\';
      Buffer.add_char b.buf escape

let ansi_c lx b =
  b.is_unquoted <- false;
  let rec read truncated =
    match peek lx 0 with
    | None -> Error "unterminated $'...' string"
    | Some '\'' ->
        advance lx 1;
        Ok ()
    | Some '\\' -> (
        match peek lx 1 with
        | None -> Error "unterminated $'...' string"
        | Some escape ->
            let start = Buffer.length b.buf in
            advance lx 2;
            decode_escape lx b escape;
            let nul = Buffer.length b.buf > start && Buffer.nth b.buf start = '\000' in
            if nul then review lx.sink (unreadable "an ANSI-C quote contains NUL");
            if truncated || nul then Buffer.truncate b.buf start;
            read (truncated || nul))
    | Some c ->
        if not truncated then Buffer.add_char b.buf c;
        advance lx 1;
        read truncated
  in
  read false

let single_quoted lx b =
  match String.index_from_opt lx.source lx.pos '\'' with
  | None -> Error "unterminated single quote"
  | Some stop ->
      add_quoted_string b (String.sub lx.source lx.pos (stop - lx.pos));
      lx.pos <- stop + 1;
      Ok ()

type mode =
  | Command
  | Argument
  | Case_subject
  | Case_word
  | Pattern
  | For_name
  | For_words
  | Function_name

type frame = Group | Resume of word list * mode

type parser = {
  mutable mode : mode;
  mutable redirected : bool;
  mutable current : word list;
  mutable frames : frame list;
  mutable cases : int;
}

let is_reserved w name = w.unquoted && String.equal w.value name

let finish lx p =
  match p.current with
  | [] -> ()
  | words ->
      lx.sink.commands <- (lx.depth, List.rev words) :: lx.sink.commands;
      p.current <- []

let on_command_word p w =
  if w.unquoted && List.mem w.value reserved_starters then ()
  else if is_reserved w "case" then (
    p.cases <- p.cases + 1;
    p.mode <- Case_subject)
  else if is_reserved w "for" || is_reserved w "select" then p.mode <- For_name
  else if is_reserved w "function" then p.mode <- Function_name
  else if is_reserved w "esac" then p.cases <- p.cases - 1
  else if not w.assignment then (
    p.current <- [ w ];
    p.mode <- Argument)

let on_word p w =
  match p.mode with
  | Command -> on_command_word p w
  | Argument -> p.current <- w :: p.current
  | Case_subject -> p.mode <- Case_word
  | Case_word -> p.mode <- Pattern
  | Pattern ->
      if is_reserved w "esac" then (
        p.cases <- p.cases - 1;
        p.mode <- Command)
  | For_name -> p.mode <- For_words
  | For_words -> if is_reserved w "do" then p.mode <- Command
  | Function_name -> p.mode <- Command

let close_group lx p =
  finish lx p;
  match p.frames with
  | [] -> `Close
  | Group :: rest ->
      p.frames <- rest;
      p.mode <- Command;
      `Continue
  | Resume (saved, mode) :: rest ->
      p.frames <- rest;
      p.current <- saved;
      p.mode <- mode;
      `Continue

let on_op lx p op =
  match (p.mode, op) with
  | Pattern, ("(" | "|") | (Case_subject | Case_word | Pattern), "\n" -> `Continue
  | Pattern, ")" ->
      p.mode <- Command;
      `Continue
  | _, "(" ->
      finish lx p;
      p.frames <- Group :: p.frames;
      p.mode <- Command;
      `Continue
  | _, ")" -> close_group lx p
  | _, (";;" | ";&" | ";;&") ->
      finish lx p;
      p.mode <- (if p.cases > 0 then Pattern else Command);
      `Continue
  | _, _ ->
      finish lx p;
      p.mode <- Command;
      `Continue

let contains_ansi_quote source = contains_substring ~needle:"$'" source

let double_quoted_parameter_word lx =
  let rec end_of_name offset =
    match peek lx offset with
    | Some c when is_name_char c -> end_of_name (offset + 1)
    | _ -> offset
  in
  let operator =
    match peek lx 0 with
    | Some c when is_name_char c -> end_of_name 1
    | Some ('@' | '*' | '#' | '?' | '-' | '$' | '!') -> 1
    | _ -> 0
  in
  let operator = if peek lx operator = Some ':' then operator + 1 else operator in
  operator > 0
  && match peek lx operator with Some ('-' | '+' | '=' | '?') -> true | _ -> false

let rec lex_word lx b =
  match peek lx 0 with
  | None -> Ok ()
  | Some c when is_metachar c -> Ok ()
  | Some '\\' -> (
      match peek lx 1 with
      | Some '\n' ->
          advance lx 2;
          lex_word lx b
      | Some c ->
          add_quoted b c;
          advance lx 2;
          lex_word lx b
      | None ->
          add_quoted b '\\';
          advance lx 1;
          Ok ())
  | Some '\'' ->
      advance lx 1;
      let* () = single_quoted lx b in
      lex_word lx b
  | Some '"' ->
      advance lx 1;
      let* () = double_quoted lx b ~closing:true in
      lex_word lx b
  | Some '$' when Option.is_some lx.delimiter_next -> (
      match peek lx 1 with
      | Some ('\'' | '"' | '{' | '(') -> Error "unsupported here-document delimiter"
      | _ ->
          Buffer.add_char b.buf '$';
          advance lx 1;
          lex_word lx b)
  | Some '`' when Option.is_some lx.delimiter_next ->
      Error "unsupported here-document delimiter"
  | Some '$' ->
      let* () = dollar lx b ~in_double:false in
      lex_word lx b
  | Some '`' ->
      advance lx 1;
      let* () = backquoted lx b ~in_double:false in
      lex_word lx b
  | Some c ->
      add_unquoted b c;
      advance lx 1;
      lex_word lx b

and double_quoted lx b ~closing =
  b.is_unquoted <- false;
  match peek lx 0 with
  | None -> if closing then Error "unterminated double quote" else Ok ()
  | Some '"' when closing ->
      advance lx 1;
      Ok ()
  | Some '\\' ->
      (match peek lx 1 with
      | Some '\n' -> advance lx 2
      | Some (('$' | '`' | '\\') as c) ->
          add_quoted b c;
          advance lx 2
      | Some '"' when closing ->
          add_quoted b '"';
          advance lx 2
      | Some _ | None ->
          add_quoted b '\\';
          advance lx 1);
      double_quoted lx b ~closing
  | Some '$' when Option.is_none lx.delimiter_next ->
      let* () = dollar lx b ~in_double:true in
      double_quoted lx b ~closing
  | Some '`' when Option.is_none lx.delimiter_next ->
      advance lx 1;
      let* () = backquoted lx b ~in_double:closing in
      double_quoted lx b ~closing
  | Some c ->
      add_quoted b c;
      advance lx 1;
      double_quoted lx b ~closing

and dollar lx b ~in_double =
  match peek lx 1 with
  | Some '\'' when lx.ansi_quotes && not in_double ->
      advance lx 2;
      ansi_c lx b
  | Some '"' when not in_double ->
      advance lx 2;
      double_quoted lx b ~closing:true
  | Some '(' ->
      let* () =
        nest lx (fun () ->
            match peek lx 2 with
            | Some '(' ->
                advance lx 3;
                arithmetic lx (builder ()) 0
            | Some _ | None ->
                advance lx 2;
                parse_list lx ~nested:true)
      in
      Ok (mark_expansion b)
  | Some '{' ->
      advance lx 2;
      let literal_apostrophe = in_double && double_quoted_parameter_word lx in
      let* () = nest lx (fun () -> parameter lx (builder ()) ~literal_apostrophe) in
      Ok (mark_expansion b)
  | Some c when is_name_start c ->
      advance lx 2;
      skip_name lx;
      Ok (mark_expansion b)
  | Some c when is_digit c || String.contains "@*#?-$!" c ->
      advance lx 2;
      Ok (mark_expansion b)
  | Some _ | None ->
      Buffer.add_char b.buf '$';
      advance lx 1;
      Ok ()

and arithmetic lx scratch depth =
  match peek lx 0 with
  | None -> Error "unterminated arithmetic expansion"
  | Some '(' ->
      advance lx 1;
      arithmetic lx scratch (depth + 1)
  | Some ')' when depth > 0 ->
      advance lx 1;
      arithmetic lx scratch (depth - 1)
  | Some ')' -> (
      match peek lx 1 with
      | Some ')' ->
          advance lx 2;
          Ok ()
      | Some _ | None -> Error "unbalanced arithmetic expansion")
  | Some '$' ->
      let* () = dollar lx scratch ~in_double:true in
      arithmetic lx scratch depth
  | Some '`' ->
      advance lx 1;
      let* () = backquoted lx scratch ~in_double:false in
      arithmetic lx scratch depth
  | Some '\\' ->
      advance lx 2;
      arithmetic lx scratch depth
  | Some _ ->
      advance lx 1;
      arithmetic lx scratch depth

and parameter lx scratch ~literal_apostrophe =
  match peek lx 0 with
  | None -> Error "unterminated parameter expansion"
  | Some '}' ->
      advance lx 1;
      Ok ()
  | Some '\\' ->
      advance lx 2;
      parameter lx scratch ~literal_apostrophe
  | Some '\'' when not literal_apostrophe ->
      advance lx 1;
      let* () = single_quoted lx scratch in
      parameter lx scratch ~literal_apostrophe
  | Some '"' ->
      advance lx 1;
      let* () = double_quoted lx scratch ~closing:true in
      parameter lx scratch ~literal_apostrophe
  | Some '$' ->
      let* () = dollar lx scratch ~in_double:literal_apostrophe in
      parameter lx scratch ~literal_apostrophe
  | Some '`' ->
      advance lx 1;
      let* () = backquoted lx scratch ~in_double:literal_apostrophe in
      parameter lx scratch ~literal_apostrophe
  | Some _ ->
      advance lx 1;
      parameter lx scratch ~literal_apostrophe

and backquoted lx b ~in_double =
  let body = Buffer.create 32 in
  let rec collect () =
    match peek lx 0 with
    | None -> Error "unterminated backquote"
    | Some '`' ->
        advance lx 1;
        Ok ()
    | Some '\\' ->
        (match peek lx 1 with
        | Some (('$' | '`' | '\\') as c) ->
            Buffer.add_char body c;
            advance lx 2
        | Some '"' when in_double ->
            Buffer.add_char body '"';
            advance lx 2
        | Some _ | None ->
            Buffer.add_char body '\\';
            advance lx 1);
        collect ()
    | Some c ->
        Buffer.add_char body c;
        advance lx 1;
        collect ()
  in
  let* () = collect () in
  parse_source lx.sink ~depth:(lx.depth + 1) (Buffer.contents body);
  Ok (mark_expansion b)

and next_token lx =
  skip_blanks lx;
  match peek lx 0 with
  | (None | Some ('\n' | '&' | '|' | ';' | '(' | ')' | '<' | '>'))
    when Option.is_some lx.delimiter_next ->
      Error "missing here-document delimiter"
  | None when lx.heredocs <> [] -> Error "unterminated here-document"
  | None -> Ok End
  | Some '#' ->
      skip_comment lx;
      next_token lx
  | Some '\n' ->
      advance lx 1;
      read_heredocs lx;
      Ok (Op "\n")
  | Some ('&' | '|' | ';' | '(' | ')' | '<' | '>') -> Ok (operator lx)
  | Some _ ->
      let b = builder () in
      let* () = lex_word lx b in
      let w = word_of b in
      let redirects =
        match peek lx 0 with Some ('<' | '>') -> true | Some _ | None -> false
      in
      if
        Option.is_none lx.delimiter_next
        && redirects && w.unquoted
        && (is_all_digits w.value || is_brace_name w.value)
      then Ok (operator lx)
      else (
        record_word lx w;
        Ok (Word w))

and read_heredocs lx =
  let pending = List.rev lx.heredocs in
  lx.heredocs <- [];
  List.iter
    (fun doc ->
      let body = read_body lx doc in
      if doc.expands then
        expand_body lx.sink ~depth:(lx.depth + 1) ~ansi_quotes:lx.ansi_quotes body)
    pending

and expand_body sink ~depth ~ansi_quotes body =
  if depth > max_depth then review sink too_deep
  else
    let b = builder () in
    match double_quoted (lexer sink ~depth ~ansi_quotes body) b ~closing:false with
    | Ok () -> sink.words <- Buffer.contents b.buf :: sink.words
    | Error message -> review sink (unreadable message)

and parse_list lx ~nested =
  let p = { mode = Command; redirected = false; current = []; frames = []; cases = 0 } in
  let rec loop () =
    let* token = next_token lx in
    match token with
    | End -> (
        finish lx p;
        match p.frames with
        | _ when nested -> Error "unterminated command substitution"
        | [] -> Ok ()
        | _ :: _ -> Error "unbalanced parenthesis")
    | Redirect ->
        p.redirected <- true;
        loop ()
    | Word _ when p.redirected ->
        p.redirected <- false;
        loop ()
    | Word w ->
        on_word p w;
        loop ()
    | Op "(" when p.redirected ->
        p.redirected <- false;
        p.frames <- Resume (p.current, p.mode) :: p.frames;
        p.current <- [];
        p.mode <- Command;
        loop ()
    | Op op -> (
        p.redirected <- false;
        match on_op lx p op with
        | `Continue -> loop ()
        | `Close -> if nested then Ok () else Error "unbalanced parenthesis")
  in
  loop ()

and parse_source sink ~depth source =
  let parse ansi_quotes =
    match parse_list (lexer sink ~depth ~ansi_quotes source) ~nested:false with
    | Ok () -> ()
    | Error message -> review sink (unreadable message)
  in
  if depth > max_depth then review sink too_deep
  else (
    parse true;
    if contains_ansi_quote source then parse false)

let is_option w = String.length w.value > 1 && (w.value.[0] = '-' || w.value.[0] = '+')
let wrapper_syntax = "the screen cannot determine a wrapper's command"

type option_use = Flag | Operand | Unknown

let option_use ~switches ~arguments value =
  if List.mem value switches then Flag
  else if List.mem value arguments then Operand
  else if
    List.exists
      (fun option ->
        let width = String.length option in
        String.length value > width
        && String.starts_with ~prefix:option value
        && (width = 2 || value.[width] = '='))
      arguments
  then Flag
  else Unknown

let rec option_prefix ~switches ~arguments = function
  | w :: _ when not w.fixed -> Error computed_program
  | w :: rest when String.equal w.value "--" -> Ok rest
  | w :: rest when is_option w -> (
      match (option_use ~switches ~arguments w.value, rest) with
      | Flag, _ -> option_prefix ~switches ~arguments rest
      | Operand, value :: rest when value.fixed -> option_prefix ~switches ~arguments rest
      | _ -> Error wrapper_syntax)
  | words -> Ok words

let wrapper_options = function
  | "env" ->
      ( [ "-i"; "--ignore-environment"; "-v"; "--debug" ],
        [ "-u"; "--unset"; "-C"; "--chdir" ] )
  | "timeout" ->
      ( [ "--foreground"; "--preserve-status"; "-v"; "--verbose" ],
        [ "-s"; "--signal"; "-k"; "--kill-after" ] )
  | "command" -> ([ "-p" ], [])
  | "time" -> ([ "-p" ], [])
  | "nice" -> ([], [ "-n"; "--adjustment" ])
  | "stdbuf" -> ([], [ "-i"; "-o"; "-e"; "--input"; "--output"; "--error" ])
  | "setsid" -> ([ "-c"; "-f"; "-w"; "--ctty"; "--fork"; "--wait" ], [])
  | "ionice" -> ([ "-t"; "--ignore" ], [ "-c"; "--class"; "-n"; "--classdata" ])
  | "chrt" -> ([ "-b"; "-f"; "-i"; "-o"; "-r" ], [])
  | "taskset" -> ([ "-c"; "--cpu-list" ], [])
  | "flock" ->
      ( [
          "-s";
          "-x";
          "-n";
          "-u";
          "-o";
          "-F";
          "--shared";
          "--exclusive";
          "--nonblock";
          "--unlock";
          "--close";
          "--no-fork";
        ],
        [ "-w"; "--timeout"; "-E"; "--conflict-exit-code" ] )
  | "chroot" -> ([], [ "--groups"; "--userspec" ])
  | _ -> ([], [])

let rec env_command = function
  | w :: _ when not w.fixed -> Error computed_program
  | w :: rest when String.contains w.value '=' -> env_command rest
  | words -> Ok words

let wrapped_command program args =
  let args =
    match (program, args) with
    | "env", w :: rest when w.fixed && String.equal w.value "-" -> rest
    | _ -> args
  in
  let switches, arguments = wrapper_options program in
  let* words = option_prefix ~switches ~arguments args in
  match (program, words) with
  | "env", _ -> env_command words
  | "flock", _ :: mode :: _ when is_option mode -> Error wrapper_syntax
  | ("timeout" | "chroot" | "taskset" | "chrt" | "flock"), operand :: command ->
      if operand.fixed then Ok command else Error computed_program
  | ("coproc" | "script" | "runuser"), _ -> Error wrapper_syntax
  | _ -> Ok words

let xargs_switches =
  [
    "-0";
    "--null";
    "-o";
    "--open-tty";
    "-p";
    "--interactive";
    "-r";
    "--no-run-if-empty";
    "-t";
    "--verbose";
    "-x";
    "--exit";
  ]

let xargs_arguments =
  [
    "-a";
    "--arg-file";
    "-d";
    "--delimiter";
    "-E";
    "-L";
    "--max-lines";
    "-n";
    "--max-args";
    "-P";
    "--max-procs";
    "-s";
    "--max-chars";
  ]

let replacement_of flag =
  if String.equal flag "-i" || String.equal flag "--replace" then Some "{}"
  else if String.starts_with ~prefix:"--replace=" flag then Some (suffix_from flag 10)
  else if String.starts_with ~prefix:"-I" flag || String.starts_with ~prefix:"-i" flag
  then Some (suffix_from flag 2)
  else None

let xargs_grouping value =
  String.starts_with ~prefix:"-n" value
  || String.starts_with ~prefix:"-L" value
  || List.mem value [ "--max-args"; "--max-lines" ]
  || String.starts_with ~prefix:"--max-args=" value
  || String.starts_with ~prefix:"--max-lines=" value

let rec xargs_command ~grouped replacement words =
  if grouped && Option.is_some replacement then Error wrapper_syntax
  else
    match words with
    | w :: _ when not w.fixed -> Error computed_program
    | w :: rest when String.equal w.value "--" -> Ok (replacement, rest)
    | w :: token :: rest
      when String.equal w.value "-I" && token.fixed && token.value <> "" ->
        xargs_command ~grouped (Some token.value) rest
    | w :: rest when is_option w -> (
        match replacement_of w.value with
        | Some "" -> Error wrapper_syntax
        | Some token -> xargs_command ~grouped (Some token) rest
        | None -> (
            let grouped = grouped || xargs_grouping w.value in
            match
              ( option_use ~switches:xargs_switches ~arguments:xargs_arguments w.value,
                rest )
            with
            | Flag, _ -> xargs_command ~grouped replacement rest
            | Operand, value :: rest when value.fixed ->
                xargs_command ~grouped replacement rest
            | _ -> Error wrapper_syntax))
    | words -> Ok (replacement, words)

let watch_switches =
  [
    "-b";
    "--beep";
    "-c";
    "--color";
    "-C";
    "--no-color";
    "-d";
    "--differences";
    "-e";
    "--errexit";
    "-g";
    "--chgexit";
    "-p";
    "--precise";
    "-r";
    "--no-rerun";
    "-t";
    "--no-title";
    "-w";
    "--no-wrap";
  ]

let watch_arguments = [ "-n"; "--interval"; "-q"; "--equexit" ]

let rec watch_command direct = function
  | w :: _ when not w.fixed -> Error computed_program
  | w :: rest when String.equal w.value "--" -> Ok (direct, rest)
  | w :: rest when List.mem w.value [ "-x"; "--exec" ] -> watch_command true rest
  | w :: rest when is_option w -> (
      match
        (option_use ~switches:watch_switches ~arguments:watch_arguments w.value, rest)
      with
      | Flag, _ -> watch_command direct rest
      | Operand, value :: rest when value.fixed -> watch_command direct rest
      | _ -> Error wrapper_syntax)
  | words -> Ok (direct, words)

let code_flag_operand flags w =
  if List.mem w.value flags then None
  else
    List.find_map
      (fun flag ->
        let attached =
          if String.starts_with ~prefix:"--" flag then flag ^ "=" else flag
        in
        if
          String.length w.value > String.length attached
          && String.starts_with ~prefix:attached w.value
        then Some (suffix_from w.value (String.length attached))
        else None)
      flags

let takes_code_operand flags w =
  let value = w.value in
  let length = String.length value in
  let ends_short_cluster flag =
    String.length flag = 2
    && length > 2
    && value.[0] = '-'
    && value.[1] <> '-'
    && value.[length - 1] = flag.[1]
  in
  List.mem value flags || List.exists ends_short_cluster flags

let check_executable sink base program args =
  if Option.is_none sink.refused_executable then
    if List.mem program refused_programs then sink.refused_executable <- Some program
    else if program <> base then
      let matches = function
        | first :: rest when first = program ->
            has_sequence (fun word -> basename word.value) rest args
        | _ -> false
      in
      sink.refused_executable <-
        List.find_opt matches refused_sequences |> Option.map (String.concat " ")

let rec analyze sink ~depth = function
  | [] -> ()
  | name :: args ->
      if not name.fixed then review sink computed_program
      else
        let base = basename name.value in
        let program = strip_executable_suffix base in
        check_executable sink base program args;
        analyze_program sink ~depth program args

and analyze_program sink ~depth program args =
  if List.mem program posix_shells || List.mem program foreign_shells then
    shell_code sink ~depth program args
  else if List.mem program code_runners then review sink sourced_code
  else if String.equal program "trap" then trap_action sink ~depth args
  else if String.equal program "xargs" then xargs sink ~depth args
  else if String.equal program "watch" then watch sink ~depth args
  else if List.mem program wrappers then wrapped sink ~depth program args
  else
    match List.assoc_opt program exec_flags with
    | Some flags ->
        if List.exists (fun w -> not w.fixed) args then review sink computed_program;
        exec_commands sink ~depth flags args
    | None -> ()

and wrapped sink ~depth program args =
  match wrapped_command program args with
  | Ok command -> analyze sink ~depth command
  | Error reason ->
      review sink reason;
      Option.iter
        (fun flags -> code_strings sink ~depth flags args)
        (List.assoc_opt program code_flags)

and code_strings sink ~depth flags = function
  | [] -> ()
  | w :: rest -> (
      match (code_flag_operand flags w, rest) with
      | Some code, _ ->
          if w.fixed then parse_source sink ~depth:(depth + 1) code
          else review sink computed_code;
          code_strings sink ~depth flags rest
      | None, code :: rest when takes_code_operand flags w ->
          if w.fixed && code.fixed then parse_source sink ~depth:(depth + 1) code.value
          else review sink computed_code;
          code_strings sink ~depth flags rest
      | None, _ -> code_strings sink ~depth flags rest)

and xargs sink ~depth args =
  match xargs_command ~grouped:false None args with
  | Error reason -> review sink reason
  | Ok (_, []) -> ()
  | Ok (None, command) ->
      let input = { value = ""; fixed = false; unquoted = false; assignment = false } in
      analyze sink ~depth (command @ [ input ])
  | Ok (Some token, command) ->
      let command =
        List.map
          (fun word ->
            if contains_substring ~needle:token word.value then
              { word with fixed = false; assignment = false }
            else word)
          command
      in
      analyze sink ~depth command

and watch sink ~depth args =
  match watch_command false args with
  | Error reason -> review sink reason
  | Ok (_, []) -> ()
  | Ok (true, words) -> analyze sink ~depth words
  | Ok (false, words) ->
      if List.exists (fun w -> not w.fixed) words then review sink computed_code
      else
        parse_source sink ~depth:(depth + 1)
          (String.concat " " (List.map (fun w -> w.value) words))

and exec_commands sink ~depth flags = function
  | [] -> ()
  | w :: rest ->
      (if List.mem w.value flags then
         match rest with
         | name :: _ when String.contains name.value '{' -> review sink computed_program
         | _ -> analyze sink ~depth rest);
      exec_commands sink ~depth flags rest

and shell_code sink ~depth program args =
  let finish ~command_string =
    review sink (if command_string then computed_code else stdin_code)
  in
  let rec options ~command_string ~from_stdin = function
    | [] -> finish ~command_string
    | w :: _ when not w.fixed -> review sink computed_code
    | w :: rest when String.equal w.value "--" -> operand ~command_string ~from_stdin rest
    | w :: _ when String.equal w.value "-" -> review sink stdin_code
    | w :: rest when String.starts_with ~prefix:"--" w.value -> (
        match (w.value, rest) with
        | ("--rcfile" | "--init-file"), _ :: rest ->
            options ~command_string ~from_stdin rest
        | _ -> options ~command_string ~from_stdin rest)
    | w :: rest when is_option w ->
        let flags = w.value in
        let command_string =
          command_string || (flags.[0] = '-' && String.contains flags 'c')
        in
        let from_stdin = from_stdin || String.contains flags 's' in
        let rest =
          match rest with
          | _ :: after when String.contains flags 'o' || String.contains flags 'O' ->
              after
          | _ -> rest
        in
        options ~command_string ~from_stdin rest
    | words -> operand ~command_string ~from_stdin words
  and operand ~command_string ~from_stdin = function
    | [] -> finish ~command_string
    | w :: _ ->
        if not command_string then (if from_stdin then review sink stdin_code)
        else if not w.fixed then review sink computed_code
        else if List.mem program posix_shells then
          parse_source sink ~depth:(depth + 1) w.value
        else review sink foreign_code
  in
  match args with
  | [ w ]
    when (program = "bash" || program = "rbash")
         && w.fixed
         && (w.value = "--version" || w.value = "--help") ->
      ()
  | _ -> options ~command_string:false ~from_stdin:false args

and trap_action sink ~depth args =
  let args =
    match args with w :: rest when String.equal w.value "--" -> rest | _ -> args
  in
  match args with
  | [] -> ()
  | w :: _ when not w.fixed -> review sink computed_code
  | w :: _ when is_option w -> ()
  | w :: _ -> parse_source sink ~depth:(depth + 1) w.value

let rec drain sink =
  match sink.commands with
  | [] -> ()
  | (depth, words) :: rest ->
      sink.commands <- rest;
      analyze sink ~depth words;
      drain sink

type cmd_state = {
  chars : Buffer.t;
  mutable computed : bool;
  mutable started : bool;
  mutable awaiting_target : bool;
  mutable in_quotes : bool;
  mutable segment : word list;
}

let end_cmd_word sink st =
  if st.started || Buffer.length st.chars > 0 then (
    let value = Buffer.contents st.chars in
    sink.words <- value :: sink.words;
    if st.awaiting_target then st.awaiting_target <- false
    else
      st.segment <-
        { value; fixed = not st.computed; unquoted = true; assignment = false }
        :: st.segment);
  Buffer.clear st.chars;
  st.computed <- false;
  st.started <- false

let cmd_redirect sink st source index =
  if is_all_digits (Buffer.contents st.chars) && not st.computed then
    Buffer.clear st.chars
  else end_cmd_word sink st;
  st.awaiting_target <- true;
  let length = String.length source in
  let next = index + 1 in
  let next = if next < length && source.[next] = '>' then next + 1 else next in
  if next < length && source.[next] = '&' then next + 1 else next

let cmd_quoted st c =
  match c with
  | '"' -> st.in_quotes <- false
  | '%' | '!' ->
      st.computed <- true;
      Buffer.add_char st.chars c
  | _ -> Buffer.add_char st.chars c

let rec parse_cmd sink ~depth source =
  if depth > max_depth then review sink too_deep
  else
    let st =
      {
        chars = Buffer.create 16;
        computed = false;
        started = false;
        awaiting_target = false;
        in_quotes = false;
        segment = [];
      }
    in
    let length = String.length source in
    let rec loop index =
      if index >= length then end_segment sink ~depth st
      else if st.in_quotes then (
        cmd_quoted st source.[index];
        loop (index + 1))
      else loop (cmd_char sink ~depth st source index)
    in
    loop 0

and cmd_char sink ~depth st source index =
  match source.[index] with
  | '"' ->
      st.in_quotes <- true;
      st.started <- true;
      index + 1
  | '^' ->
      (if index + 1 < String.length source then
         match source.[index + 1] with
         | '\n' | '\r' -> ()
         | next -> Buffer.add_char st.chars next);
      index + 2
  | ('%' | '!') as c ->
      st.computed <- true;
      Buffer.add_char st.chars c;
      index + 1
  | ' ' | '\t' | ',' | ';' | '=' | '\011' | '\012' ->
      end_cmd_word sink st;
      index + 1
  | '&' | '|' | '(' | ')' | '\n' | '\r' ->
      end_segment sink ~depth st;
      index + 1
  | '<' | '>' -> cmd_redirect sink st source index
  | c ->
      Buffer.add_char st.chars c;
      index + 1

and end_segment sink ~depth st =
  end_cmd_word sink st;
  let words = List.rev st.segment in
  st.segment <- [];
  st.awaiting_target <- false;
  analyze_cmd sink ~depth words

and analyze_cmd sink ~depth = function
  | [] -> ()
  | name :: args ->
      if not name.fixed then review sink computed_program
      else
        let base = basename (drop_leading '@' name.value) in
        let program = strip_executable_suffix base in
        check_executable sink base program args;
        if List.mem program cmd_runners then
          List.iter
            (fun w ->
              if w.fixed then parse_cmd sink ~depth:(depth + 1) w.value
              else review sink computed_program)
            args

let screen dialect command =
  let sink = { words = []; commands = []; reviews = []; refused_executable = None } in
  (match dialect with
  | Posix ->
      parse_source sink ~depth:0 command;
      drain sink
  | Cmd -> parse_cmd sink ~depth:0 command);
  let names text = List.map basename (fragments dialect text) in
  match refused (names command) with
  | Some program -> Blocked program
  | None -> (
      match refused (List.concat_map names (List.rev sink.words)) with
      | Some program -> Blocked program
      | None -> (
          match sink.refused_executable with
          | Some program -> Blocked program
          | None -> (
              match List.rev sink.reviews with
              | reason :: _ -> Review reason
              | [] -> Clear)))
