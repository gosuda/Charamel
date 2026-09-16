type compiled = {
  line_comments : string list;
  block_comments : (string * string) list;
  strings : (string * string * bool) list;
  raw_strings : (string * string) list;
  words : (string * Spec.kind) list;
  number : Re.re;
  ident : Re.re;
  operators : string list;
  attribute : Re.re option;
  case_sensitive : bool;
}

let nonempty s = String.length s > 0
let compare_longest a b = compare (String.length b) (String.length a)
let sort_longest xs = List.sort compare_longest (List.filter nonempty xs)
let sort_pairs xs = List.sort (fun (a, _) (b, _) -> compare_longest a b) xs
let sort_triples xs = List.sort (fun (a, _, _) (b, _, _) -> compare_longest a b) xs

let word_priority = function
  | Spec.Constant -> 0
  | Spec.Keyword -> 1
  | Spec.Type -> 2
  | Spec.Builtin -> 3
  | Spec.String | Spec.Number | Spec.Comment | Spec.Operator | Spec.Punct | Spec.Ident
  | Spec.Attribute | Spec.Text ->
      4

(* A word declared in more than one category (for example TypeScript's [undefined] as
   both a keyword and a constant) keeps only its highest-precedence classification, so
   the scanner never depends on sort stability to break the tie. *)
let dedup_words words =
  let table = Hashtbl.create (List.length words) in
  List.iter
    (fun (word, kind) ->
      match Hashtbl.find_opt table word with
      | Some existing when word_priority existing <= word_priority kind -> ()
      | _ -> Hashtbl.replace table word kind)
    words;
  Hashtbl.fold (fun word kind acc -> (word, kind) :: acc) table []

let compile (spec : Spec.t) =
  let words kind values =
    List.map (fun word -> (word, kind)) (List.filter nonempty values)
  in
  let words =
    List.concat
      [
        words Spec.Keyword spec.keywords;
        words Spec.Type spec.types;
        words Spec.Builtin spec.builtins;
        words Spec.Constant spec.constants;
      ]
  in
  let words =
    if spec.case_sensitive then words
    else List.map (fun (word, kind) -> (String.lowercase_ascii word, kind)) words
  in
  let words = dedup_words words in
  {
    line_comments = sort_longest spec.line_comment;
    block_comments = sort_pairs spec.block_comment;
    strings = sort_triples spec.strings;
    raw_strings = sort_pairs spec.raw_strings;
    words = sort_pairs words;
    number = Re.compile (Re.longest spec.number);
    ident = Re.compile (Re.longest spec.ident);
    operators = sort_longest spec.operators;
    attribute = Option.map (fun regex -> Re.compile (Re.longest regex)) spec.attribute;
    case_sensitive = spec.case_sensitive;
  }

(* One compiled record per live [Spec.t], memoized per domain. The table is
   domain-local so no [Re.re] is ever shared across domains, and its keys are
   weak so an entry dies with the specification that owns it -- a transient
   spec cannot leak. Keys are compared by physical identity ([==]); [names] is
   an immutable field of the record and only buckets the hash, so two distinct
   specs that happen to share a [names] list still get separate entries. *)
module Compiled_table = Ephemeron.K1.Make (struct
  type t = Spec.t

  let equal a b = a == b
  let hash spec = Hashtbl.hash spec.Spec.names
end)

let cache = Domain.DLS.new_key (fun () -> Compiled_table.create 16)

let compiled_for (spec : Spec.t) =
  let table = Domain.DLS.get cache in
  match Compiled_table.find_opt table spec with
  | Some compiled -> compiled
  | None ->
      let compiled = compile spec in
      Compiled_table.replace table spec compiled;
      compiled

let prefix_at source position literal =
  let literal_length = String.length literal in
  let source_length = String.length source in
  if literal_length = 0 || position < 0 || position + literal_length > source_length then
    false
  else
    let rec equal offset =
      offset = literal_length
      || (source.[position + offset] = literal.[offset] && equal (offset + 1))
    in
    equal 0

let prefix_folded source position literal =
  let literal_length = String.length literal in
  let source_length = String.length source in
  if literal_length = 0 || position < 0 || position + literal_length > source_length then
    false
  else
    let rec equal offset =
      offset = literal_length
      || Char.lowercase_ascii source.[position + offset] = literal.[offset]
         && equal (offset + 1)
    in
    equal 0

let regex_end regex source position =
  match Re.exec_opt ~pos:position regex source with
  | None -> None
  | Some groups ->
      let start, stop = Re.Group.offset groups 0 in
      if start = position && stop > position then Some stop else None

let line_end source position =
  let length = String.length source in
  let rec loop index =
    if index >= length || source.[index] = '\n' then index else loop (index + 1)
  in
  loop position

let delimited_end source position opening closing ~escapes =
  let length = String.length source in
  let closing_length = String.length closing in
  let rec loop index =
    if index >= length then length
    else if escapes && source.[index] = '\\' then
      if index + 1 >= length then length else loop (index + 2)
    else if prefix_at source index closing then index + closing_length
    else loop (index + 1)
  in
  loop (position + String.length opening)

let quoted_raw_end source position =
  let length = String.length source in
  if position >= length || source.[position] <> '{' then None
  else
    let rec bar index =
      if index >= length || source.[index] = '\n' then None
      else if source.[index] = '|' then Some index
      else if
        (source.[index] >= 'a' && source.[index] <= 'z')
        || (source.[index] >= '0' && source.[index] <= '9')
        || source.[index] = '_'
      then bar (index + 1)
      else None
    in
    match bar (position + 1) with
    | None -> None
    | Some separator ->
        let id = String.sub source (position + 1) (separator - position - 1) in
        let closing = "|" ^ id ^ "}" in
        Some (delimited_end source position "{" closing ~escapes:false)

let string_end strings source position =
  let rec find = function
    | [] -> None
    | (opening, closing, escapes) :: rest ->
        if prefix_at source position opening then
          Some (delimited_end source position opening closing ~escapes)
        else find rest
  in
  find strings

let hash_raw_end raw_strings source position =
  let rec find = function
    | [] -> None
    | (opening, _) :: rest -> (
        match String.index_opt opening '#' with
        | None -> find rest
        | Some first_hash ->
            let rec hash_end index =
              if index < String.length opening && opening.[index] = '#' then
                hash_end (index + 1)
              else index
            in
            let declared_hash_end = hash_end first_hash in
            let prefix = String.sub opening 0 first_hash in
            let quote =
              String.sub opening declared_hash_end
                (String.length opening - declared_hash_end)
            in
            if quote = "" || (prefix <> "" && not (prefix_at source position prefix)) then
              find rest
            else
              let after_prefix = position + String.length prefix in
              let rec source_hashes index count =
                if index < String.length source && source.[index] = '#' then
                  source_hashes (index + 1) (count + 1)
                else count
              in
              let count = source_hashes after_prefix 0 in
              if
                count < declared_hash_end - first_hash
                || not (prefix_at source (after_prefix + count) quote)
              then find rest
              else
                let opening_length = String.length prefix + count + String.length quote in
                let opening = String.sub source position opening_length in
                let closing = quote ^ String.make count '#' in
                Some (delimited_end source position opening closing ~escapes:false))
  in
  find raw_strings

let raw_end raw_strings source position =
  match hash_raw_end raw_strings source position with
  | Some stop -> Some stop
  | None -> (
      match quoted_raw_end source position with
      | Some stop when List.exists (fun (opening, _) -> opening = "{|") raw_strings ->
          Some stop
      | _ ->
          let rec find = function
            | [] -> None
            | (opening, closing) :: rest ->
                if prefix_at source position opening then
                  Some (delimited_end source position opening closing ~escapes:false)
                else find rest
          in
          find raw_strings)

let block_end strings raw_strings source position opening closing =
  let source_length = String.length source in
  let opening_length = String.length opening in
  let closing_length = String.length closing in
  let nested = opening = "(*" in
  let rec loop index depth =
    if index >= source_length then source_length
    else if nested && prefix_at source index opening then
      loop (index + opening_length) (depth + 1)
    else if prefix_at source index closing then
      if depth = 1 then index + closing_length
      else loop (index + closing_length) (depth - 1)
    else if nested then
      match raw_end raw_strings source index with
      | Some stop -> loop stop depth
      | None -> (
          match string_end strings source index with
          | Some stop -> loop stop depth
          | None -> loop (index + 1) depth)
    else loop (index + 1) depth
  in
  loop (position + opening_length) 1

let block_end_for blocks strings raw_strings source position =
  let rec find = function
    | [] -> None
    | (opening, closing) :: rest ->
        if prefix_at source position opening then
          Some (block_end strings raw_strings source position opening closing)
        else find rest
  in
  find blocks

let identifier_byte character =
  (character >= 'A' && character <= 'Z')
  || (character >= 'a' && character <= 'z')
  || (character >= '0' && character <= '9')
  || character = '_' || character = '\'' || character = '$' || character = '-'
  || character = '?' || character = '!'

let identifier_start source position =
  let index = ref position in
  while !index > 0 && identifier_byte source.[!index - 1] do
    decr index
  done;
  !index

let word_end words ident source position case_sensitive =
  let start = identifier_start source position in
  let left_boundary =
    start = position
    ||
    match regex_end ident source start with
    | Some stop -> stop <= position
    | None -> true
  in
  if not left_boundary then None
  else
    (* When [position] itself begins an identifier per this language's own [ident]
       rule, a candidate word is only a real boundary match once it reaches at least
       as far as that natural extent -- this lets a Rust macro call like [println!]
       stop the word at the ['!'] (absent from Rust's ident tail) instead of the
       generic byte superset in [identifier_byte] swallowing it as an
       identifier-continuation byte, while a longer identifier such as [foo1]
       (rejecting a bare keyword prefix) still correctly extends past the word, and
       a word whose own text embeds a suffix such as Ruby's ["defined?"] still passes
       because its length already exceeds the natural extent. Only words that do not
       start an identifier at all here (C's ["#include"]) keep the original
       generic-byte boundary check unchanged. *)
    let natural_stop = regex_end ident source position in
    let rec find = function
      | [] -> None
      | (word, kind) :: rest ->
          let matched =
            if case_sensitive then prefix_at source position word
            else prefix_folded source position word
          in
          if matched then
            let stop = position + String.length word in
            let boundary =
              match natural_stop with
              | Some natural -> stop >= natural
              | None -> stop = String.length source || not (identifier_byte source.[stop])
            in
            if boundary then Some (kind, stop) else find rest
          else find rest
    in
    find words

let operator_end operators source position =
  let rec find = function
    | [] -> None
    | operator :: rest ->
        if prefix_at source position operator then Some (position + String.length operator)
        else find rest
  in
  find operators

let attribute_end attribute source position =
  match attribute with None -> None | Some regex -> regex_end regex source position

let add_candidate position priority kind stop candidates =
  if stop > position then (kind, stop, priority) :: candidates else candidates

let choose_candidate candidates =
  let rec loop best = function
    | [] -> best
    | (kind, stop, priority) :: rest ->
        let best =
          match best with
          | None -> Some (kind, stop, priority)
          | Some (_, best_stop, best_priority)
            when stop > best_stop || (stop = best_stop && priority < best_priority) ->
              Some (kind, stop, priority)
          | Some _ -> best
        in
        loop best rest
  in
  loop None candidates

let step compiled source position =
  let candidates = [] in
  let candidates =
    match attribute_end compiled.attribute source position with
    | None -> candidates
    | Some stop -> add_candidate position 0 Spec.Attribute stop candidates
  in
  let candidates =
    List.fold_left
      (fun candidates marker ->
        if prefix_at source position marker then
          add_candidate position 1 Spec.Comment (line_end source position) candidates
        else candidates)
      candidates compiled.line_comments
  in
  let candidates =
    match
      block_end_for compiled.block_comments compiled.strings compiled.raw_strings source
        position
    with
    | None -> candidates
    | Some stop -> add_candidate position 1 Spec.Comment stop candidates
  in
  let candidates =
    match raw_end compiled.raw_strings source position with
    | None -> candidates
    | Some stop -> add_candidate position 2 Spec.String stop candidates
  in
  let candidates =
    match string_end compiled.strings source position with
    | None -> candidates
    | Some stop -> add_candidate position 2 Spec.String stop candidates
  in
  let candidates =
    match
      word_end compiled.words compiled.ident source position compiled.case_sensitive
    with
    | None -> candidates
    | Some (kind, stop) -> add_candidate position 3 kind stop candidates
  in
  let candidates =
    match regex_end compiled.number source position with
    | None -> candidates
    | Some stop -> add_candidate position 4 Spec.Number stop candidates
  in
  let candidates =
    match regex_end compiled.ident source position with
    | None -> candidates
    | Some stop -> add_candidate position 5 Spec.Ident stop candidates
  in
  let candidates =
    match operator_end compiled.operators source position with
    | None -> candidates
    | Some stop -> add_candidate position 6 Spec.Operator stop candidates
  in
  let candidates =
    if String.contains "(){}[],;.:" source.[position] then
      add_candidate position 7 Spec.Punct (position + 1) candidates
    else candidates
  in
  match choose_candidate candidates with
  | None -> None
  | Some (kind, stop, _) -> Some (kind, stop)

let merge_text spans =
  let rec loop acc = function
    | [] -> List.rev acc
    | ((Spec.Text, first, last) as current) :: rest -> (
        match acc with
        | (Spec.Text, previous_first, previous_last) :: tail when previous_last = first ->
            loop ((Spec.Text, previous_first, last) :: tail) rest
        | _ -> loop (current :: acc) rest)
    | current :: rest -> loop (current :: acc) rest
  in
  loop [] spans

let tokenize_compiled compiled source =
  let source_length = String.length source in
  let rec scan position spans =
    if position >= source_length then List.rev spans
    else
      match step compiled source position with
      | Some (kind, stop) when stop > position ->
          scan stop ((kind, position, stop) :: spans)
      | _ -> scan (position + 1) ((Spec.Text, position, position + 1) :: spans)
  in
  let spans = merge_text (scan 0 []) in
  let rec project acc = function
    | [] -> List.rev acc
    | (kind, first, last) :: rest ->
        project ((kind, String.sub source first (last - first)) :: acc) rest
  in
  project [] spans

let tokenize (spec : Spec.t) source = tokenize_compiled (compiled_for spec) source
