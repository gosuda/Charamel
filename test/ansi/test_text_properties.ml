(* Property complement to the upstream golden vectors in [test_text.ml]. The vectors
   pin the shared table cell by cell; these invariants must hold for every input,
   including the ones no upstream table lists: the budget is never exceeded, a
   transformation is stable under repetition, and no cut ever splits an escape
   sequence into visible bytes. *)

module Text = Charamel_ansi.Text

let sgr_sequences = [ "\x1b[31m"; "\x1b[1m"; "\x1b[m"; "\x1b[0m"; "\x1b[38;5;9m" ]
let wide_clusters = [ "你"; "好"; "漢"; "字"; "→"; "①" ]
let combining_clusters = [ "e\xCC\x81"; "a\xCC\x81\xCC\x82" ]

let letter =
  QCheck2.Gen.map
    (fun code -> String.make 1 (Char.chr code))
    (QCheck2.Gen.int_range 97 122)

let atom =
  QCheck2.Gen.oneof_weighted
    [
      (10, letter);
      (2, QCheck2.Gen.return " ");
      (3, QCheck2.Gen.oneof_list wide_clusters);
      (1, QCheck2.Gen.oneof_list combining_clusters);
      (2, QCheck2.Gen.oneof_list sgr_sequences);
      (2, QCheck2.Gen.return "\n");
    ]

let flat_atom =
  QCheck2.Gen.oneof_weighted
    [
      (10, letter);
      (2, QCheck2.Gen.return " ");
      (3, QCheck2.Gen.oneof_list wide_clusters);
      (1, QCheck2.Gen.oneof_list combining_clusters);
      (2, QCheck2.Gen.oneof_list sgr_sequences);
    ]

let gen_text =
  QCheck2.Gen.map
    (fun atoms -> String.concat "" atoms)
    (QCheck2.Gen.list_size (QCheck2.Gen.int_range 0 14) atom)

let gen_flat =
  QCheck2.Gen.map
    (fun atoms -> String.concat "" atoms)
    (QCheck2.Gen.list_size (QCheck2.Gen.int_range 0 14) flat_atom)

let gen_width = QCheck2.Gen.int_range 0 12
let gen_positive_width = QCheck2.Gen.int_range 1 12

let widest_cluster text =
  List.fold_left
    (fun best cluster -> max best (Charamel_ansi.Width.grapheme_width cluster))
    0
    (Charamel_ansi.Width.graphemes text)

let breaks_no_sequence text = not (String.contains (Text.strip text) '\027')

let every_line_is text ~test =
  List.for_all (fun line -> test line) (String.split_on_char '\n' text)

let truncate_stays_inside_the_budget =
  QCheck2.Test.make ~name:"truncate stays inside the budget"
    (QCheck2.Gen.pair gen_text gen_width) (fun (text, width) ->
      Text.width (Text.truncate ~width text) <= max 0 width)

let truncate_is_idempotent =
  QCheck2.Test.make ~name:"truncate is idempotent" (QCheck2.Gen.pair gen_text gen_width)
    (fun (text, width) ->
      let once = Text.truncate ~width text in
      Text.truncate ~width once = once)

let a_truncation_that_fits_is_the_identity =
  QCheck2.Test.make ~name:"a truncation that fits is the identity"
    (QCheck2.Gen.pair gen_text gen_width) (fun (text, width) ->
      width < Text.width text || Text.truncate ~width text = text)

let the_visible_text_of_a_truncation_is_a_prefix =
  QCheck2.Test.make ~name:"the visible text of a truncation is a prefix"
    (QCheck2.Gen.pair gen_text gen_width) (fun (text, width) ->
      String.starts_with
        ~prefix:(Text.strip (Text.truncate ~width text))
        (Text.strip text))

let truncate_breaks_no_sequence =
  QCheck2.Test.make ~name:"truncate breaks no sequence"
    (QCheck2.Gen.pair gen_text gen_width) (fun (text, width) ->
      breaks_no_sequence (Text.truncate ~width text))

let truncate_left_breaks_no_sequence =
  QCheck2.Test.make ~name:"truncate_left breaks no sequence"
    (QCheck2.Gen.pair gen_text gen_width) (fun (text, width) ->
      breaks_no_sequence (Text.truncate_left ~width text))

let pad_right_reaches_the_requested_width =
  QCheck2.Test.make ~name:"pad_right reaches the requested width"
    (QCheck2.Gen.pair gen_flat gen_width) (fun (text, width) ->
      Text.width (Text.pad_right ~width text) = max width (Text.width text))

let pad_right_is_idempotent =
  QCheck2.Test.make ~name:"pad_right is idempotent" (QCheck2.Gen.pair gen_flat gen_width)
    (fun (text, width) ->
      let once = Text.pad_right ~width text in
      Text.pad_right ~width once = once)

let hardwrap_keeps_every_line_inside_the_box =
  QCheck2.Test.make ~name:"hardwrap keeps every line inside the box"
    (QCheck2.Gen.pair gen_text gen_positive_width) (fun (text, width) ->
      let bound = max width (widest_cluster text) in
      every_line_is (Text.hardwrap ~width text) ~test:(fun line ->
          Text.width line <= bound))

let hardwrap_is_idempotent =
  QCheck2.Test.make ~name:"hardwrap is idempotent"
    (QCheck2.Gen.pair gen_text gen_positive_width) (fun (text, width) ->
      let once = Text.hardwrap ~width text in
      Text.hardwrap ~width once = once)

let wrap_keeps_every_line_inside_the_box =
  QCheck2.Test.make ~name:"wrap keeps every line inside the box"
    (QCheck2.Gen.pair gen_text gen_positive_width) (fun (text, width) ->
      let bound = max width (widest_cluster text) in
      every_line_is (Text.wrap ~width text) ~test:(fun line -> Text.width line <= bound))

let wrap_is_idempotent =
  QCheck2.Test.make ~name:"wrap is idempotent"
    (QCheck2.Gen.pair gen_text gen_positive_width) (fun (text, width) ->
      let once = Text.wrap ~width text in
      Text.wrap ~width once = once)

let wrap_breaks_no_sequence =
  QCheck2.Test.make ~name:"wrap breaks no sequence"
    (QCheck2.Gen.pair gen_text gen_positive_width) (fun (text, width) ->
      breaks_no_sequence (Text.wrap ~width text))

let tests =
  [
    truncate_stays_inside_the_budget;
    truncate_is_idempotent;
    a_truncation_that_fits_is_the_identity;
    the_visible_text_of_a_truncation_is_a_prefix;
    truncate_breaks_no_sequence;
    truncate_left_breaks_no_sequence;
    pad_right_reaches_the_requested_width;
    pad_right_is_idempotent;
    hardwrap_keeps_every_line_inside_the_box;
    hardwrap_is_idempotent;
    wrap_keeps_every_line_inside_the_box;
    wrap_is_idempotent;
    wrap_breaks_no_sequence;
  ]

let rand = Random.State.make [| 20260927 |]

let cases =
  List.map (fun test -> QCheck_alcotest.to_alcotest ~rand ~speed_level:`Quick test) tests
