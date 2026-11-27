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

let operand_wrappers = [ "timeout"; "chroot"; "taskset"; "chrt"; "flock" ]

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

let xargs_argument_options = [ "-I"; "-a"; "-d"; "-E"; "-L"; "-n"; "-P"; "-s" ]
let cmd_runners = [ "if"; "for"; "do"; "else"; "call"; "start"; "cmd" ]
let executable_suffixes = [ ".exe"; ".com"; ".bat"; ".cmd" ]

let reserved_starters =
  [ "!"; "{"; "}"; "if"; "then"; "else"; "elif"; "fi"; "do"; "done"; "while"; "until" ]

let computed_program = "the shell computes the name of the program it runs"
let for_without_do = "a for loop without do"
let for_command_source = "a for /f loop runs a command the screen does not parse"
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

let program_name text =
  let name = String.lowercase_ascii text in
  let start =
    match (String.rindex_opt name '/', String.rindex_opt name '\\') with
    | Some slash, Some backslash -> max slash backslash + 1
    | Some index, None | None, Some index -> index + 1
    | None, None -> 0
  in
  let base = drop_leading '=' (suffix_from name start) in
  match
    List.find_opt
      (fun suffix ->
        String.length base > String.length suffix && String.ends_with ~suffix base)
      executable_suffixes
  with
  | Some suffix -> String.sub base 0 (String.length base - String.length suffix)
  | None -> base

let rec has_sequence wanted names =
  match (wanted, names) with
  | [], _ -> true
  | _ :: _, [] -> false
  | first :: rest, name :: tail ->
      if String.equal name first then has_sequence rest tail else has_sequence wanted tail

let refused names =
  match List.find_opt (fun name -> List.mem name refused_programs) names with
  | Some name -> Some name
  | None ->
      List.find_opt (fun sequence -> has_sequence sequence names) refused_sequences
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
  mutable for_open : bool;
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

let read_body lx doc =
  let length = String.length lx.source in
  let rec lines acc =
    if lx.pos >= length then acc
    else
      let stop =
        Option.value (String.index_from_opt lx.source lx.pos '\n') ~default:length
      in
      let line = String.sub lx.source lx.pos (stop - lx.pos) in
      lx.pos <- min length (stop + 1);
      let line = if doc.strip_tabs then drop_leading '\t' line else line in
      if String.equal line doc.delimiter then acc else lines (line :: acc)
  in
  String.concat "\n" (List.rev (lines []))

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

let rec ansi_c lx b =
  b.is_unquoted <- false;
  match peek lx 0 with
  | None -> Error "unterminated $'...' string"
  | Some '\'' ->
      advance lx 1;
      Ok ()
  | Some '\\' -> (
      match peek lx 1 with
      | None -> Error "unterminated $'...' string"
      | Some escape ->
          advance lx 2;
          decode_escape lx b escape;
          ansi_c lx b)
  | Some c ->
      Buffer.add_char b.buf c;
      advance lx 1;
      ansi_c lx b

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
  | Some '$' ->
      let* () = dollar lx b ~in_double:true in
      double_quoted lx b ~closing
  | Some '`' ->
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
      let* () = nest lx (fun () -> parameter lx (builder ())) in
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

and parameter lx scratch =
  match peek lx 0 with
  | None -> Error "unterminated parameter expansion"
  | Some '}' ->
      advance lx 1;
      Ok ()
  | Some '\\' ->
      advance lx 2;
      parameter lx scratch
  | Some '\'' ->
      advance lx 1;
      let* () = single_quoted lx scratch in
      parameter lx scratch
  | Some '"' ->
      advance lx 1;
      let* () = double_quoted lx scratch ~closing:true in
      parameter lx scratch
  | Some '$' ->
      let* () = dollar lx scratch ~in_double:false in
      parameter lx scratch
  | Some '`' ->
      advance lx 1;
      let* () = backquoted lx scratch ~in_double:false in
      parameter lx scratch
  | Some _ ->
      advance lx 1;
      parameter lx scratch

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
      if redirects && w.unquoted && (is_all_digits w.value || is_brace_name w.value) then
        Ok (operator lx)
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

let rec wrapped_command ~operands = function
  | [] -> []
  | w :: rest when w.assignment || is_option w || is_all_digits w.value ->
      wrapped_command ~operands rest
  | _ :: rest when operands > 0 -> wrapped_command ~operands:(operands - 1) rest
  | words -> words

let replacement_of flag =
  if String.equal flag "-i" || String.equal flag "--replace" then Some "{}"
  else if String.starts_with ~prefix:"--replace=" flag then Some (suffix_from flag 10)
  else if String.starts_with ~prefix:"-I" flag || String.starts_with ~prefix:"-i" flag
  then Some (suffix_from flag 2)
  else None

let rec xargs_command replacement = function
  | [] -> (replacement, [])
  | w :: rest when String.equal w.value "--" -> (replacement, rest)
  | w :: token :: rest when String.equal w.value "-I" ->
      xargs_command (Some token.value) rest
  | w :: _ :: rest when List.mem w.value xargs_argument_options ->
      xargs_command replacement rest
  | w :: rest when is_option w ->
      let replacement =
        match replacement_of w.value with Some token -> Some token | None -> replacement
      in
      xargs_command replacement rest
  | words -> (replacement, words)

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

let rec analyze sink ~depth = function
  | [] -> ()
  | name :: args ->
      if not name.fixed then review sink computed_program
      else analyze_program sink ~depth (program_name name.value) args

and analyze_program sink ~depth program args =
  if List.mem program posix_shells then shell_code sink ~depth ~readable:true args
  else if List.mem program foreign_shells then shell_code sink ~depth ~readable:false args
  else if List.mem program code_runners then review sink sourced_code
  else if String.equal program "trap" then trap_action sink ~depth args
  else if String.equal program "xargs" then xargs sink ~depth args
  else if String.equal program "watch" then watch sink ~depth args
  else if List.mem program wrappers then wrapped sink ~depth program args
  else
    match List.assoc_opt program exec_flags with
    | Some flags -> exec_commands sink ~depth flags args
    | None -> ()

and wrapped sink ~depth program args =
  if List.exists (fun w -> not (w.fixed || w.assignment)) args then
    review sink computed_program
  else (
    Option.iter
      (fun flags -> code_strings sink ~depth flags args)
      (List.assoc_opt program code_flags);
    let operands = if List.mem program operand_wrappers then 1 else 0 in
    analyze sink ~depth (wrapped_command ~operands args))

and code_strings sink ~depth flags = function
  | [] -> ()
  | w :: rest -> (
      match (code_flag_operand flags w, rest) with
      | Some code, _ ->
          parse_source sink ~depth:(depth + 1) code;
          code_strings sink ~depth flags rest
      | None, code :: rest when takes_code_operand flags w ->
          parse_source sink ~depth:(depth + 1) code.value;
          code_strings sink ~depth flags rest
      | None, _ -> code_strings sink ~depth flags rest)

and xargs sink ~depth args =
  if List.exists (fun w -> not w.fixed) args then review sink computed_program
  else
    match xargs_command None args with
    | Some token, name :: _ when contains_substring ~needle:token name.value ->
        review sink computed_program
    | _, command -> analyze sink ~depth command

and watch sink ~depth args =
  let rec command = function
    | w :: _ :: rest when String.equal w.value "-n" || String.equal w.value "--interval"
      ->
        command rest
    | w :: rest when is_option w -> command rest
    | words -> words
  in
  match command args with
  | [] -> ()
  | words when List.exists (fun w -> not w.fixed) words -> review sink computed_code
  | words ->
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

and shell_code sink ~depth ~readable args =
  let rec options ~command_string ~from_stdin = function
    | [] -> if not command_string then review sink stdin_code
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
    | [] -> if not command_string then review sink stdin_code
    | w :: _ ->
        if not command_string then (if from_stdin then review sink stdin_code)
        else if not w.fixed then review sink computed_code
        else if readable then parse_source sink ~depth:(depth + 1) w.value
        else review sink foreign_code
  in
  options ~command_string:false ~from_stdin:false args

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

and for_command sink ~depth args =
  let folded = List.map (fun w -> String.lowercase_ascii w.value) args in
  let rec after_do = function
    | [] -> None
    | w :: rest ->
        if String.equal (String.lowercase_ascii w.value) "do" then Some rest
        else after_do rest
  in
  if List.mem "/f" folded then review sink for_command_source
  else
    match after_do args with
    | Some body -> analyze_cmd sink ~depth body
    | None -> sink.for_open <- true

and analyze_cmd sink ~depth = function
  | [] -> ()
  | name :: args ->
      if not name.fixed then review sink computed_program
      else
        let prog = program_name (drop_leading '@' name.value) in
        (* A [for] header names its loop variable and set, neither of which is
           code: [%f] and [%%f] expand to file names, and the set is data. The
           body after [do] is the command, analysed as one unit so an argument
           holding a loop variable is not read as a program. A header without
           [do] leaves [for_open] set, so a loop whose body never arrives stays
           under review. A [/f] header can read a command source the scanner
           does not parse, so it stays under review. *)
        if String.equal prog "for" then for_command sink ~depth args
        else if String.equal prog "do" then (
          sink.for_open <- false;
          analyze_cmd sink ~depth args)
        else if List.mem prog cmd_runners then
          List.iter
            (fun w ->
              if w.fixed then parse_cmd sink ~depth:(depth + 1) w.value
              else review sink computed_program)
            args

let screen dialect command =
  let sink = { words = []; commands = []; reviews = []; for_open = false } in
  (match dialect with
  | Posix ->
      parse_source sink ~depth:0 command;
      drain sink
  | Cmd ->
      parse_cmd sink ~depth:0 command;
      if sink.for_open then review sink for_without_do);
  let names text = List.map program_name (fragments dialect text) in
  let decoded = List.concat_map names (List.rev sink.words) in
  match refused (names command) with
  | Some program -> Blocked program
  | None -> (
      match refused decoded with
      | Some program -> Blocked program
      | None -> (
          match List.rev sink.reviews with reason :: _ -> Review reason | [] -> Clear))
