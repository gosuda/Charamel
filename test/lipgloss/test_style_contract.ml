open Charm_lipgloss

let check_string name expected actual = Alcotest.(check string) name expected actual
let check_int name expected actual = Alcotest.(check int) name expected actual
let check_bool name expected actual = Alcotest.(check bool) name expected actual

let check_int_opt name expected actual =
  Alcotest.(check (option int)) name expected actual

let check_bool_opt name expected actual =
  Alcotest.(check (option bool)) name expected actual

let color_eq = Alcotest.testable Color.pp Color.equal

let check_color_opt name expected actual =
  Alcotest.(check (option color_eq)) name expected actual

let red = Option.get (Color.rgb 255 0 0)
let blue = Option.get (Color.rgb 0 0 255)
let white = Option.get (Color.rgb 255 255 255)
let dark_bg = Option.get (Color.rgb 0x11 0x11 0x11)

(* ---------------------------------------------------------------------- *)
(* Property setting/unsetting: ports .references/lipgloss/style_test.go
   TestStyleUnset (style_test.go:234-362). PaddingChar/MarginChar lines are
   dropped (no such property exists in Style.t at all: see report). The
   per-side padding/margin Unset* lines (upstream UnsetPaddingTop etc., each
   an independent propKey) are also dropped: Style.padding/margin hold a
   single [Sides.t option], so only the whole record can be set or unset,
   never one side alone (see report). Border sides ARE independently
   settable in our design and are ported in full. *)
let test_bool_props_set_unset () =
  let check_flag name set unset get =
    let s = set true Style.empty in
    check_bool_opt (name ^ " set true") (Some true) (get s);
    check_bool_opt (name ^ " unset") None (get (unset s))
  in
  check_flag "bold" Style.bold Style.unset_bold Style.get_bold;
  check_flag "italic" Style.italic Style.unset_italic Style.get_italic;
  check_flag "underline" Style.underline Style.unset_underline Style.get_underline;
  check_flag "underline_spaces" Style.underline_spaces Style.unset_underline_spaces
    Style.get_underline_spaces;
  check_flag "strikethrough" Style.strikethrough Style.unset_strikethrough
    Style.get_strikethrough;
  check_flag "strikethrough_spaces" Style.strikethrough_spaces
    Style.unset_strikethrough_spaces Style.get_strikethrough_spaces;
  check_flag "reverse" Style.reverse Style.unset_reverse Style.get_reverse;
  check_flag "blink" Style.blink Style.unset_blink Style.get_blink;
  check_flag "faint" Style.faint Style.unset_faint Style.get_faint;
  check_flag "inline" Style.inline Style.unset_inline Style.get_inline;
  (* A property explicitly set to [false] stays SET (Some false), distinct
     from unset (None): the option-based getter design (documented at
     style.mli:121) can tell "present but false" from "absent", which
     upstream's bool-with-implicit-default getters cannot. *)
  check_bool_opt "explicit false is Some false, not None" (Some false)
    (Style.get_bold (Style.bold false Style.empty))

let test_hyperlink_render_and_inherit () =
  let link : Charm_ansi.Link.t = { url = "https://example.test"; params = [] } in
  let styled =
    Style.hyperlink link
      (Style.padding (Sides.all 1) (Style.border Border.normal Style.empty))
  in
  check_bool "hyperlink getter returns the typed link" true
    (Style.get_hyperlink (Style.hyperlink link Style.empty) = Some link);
  check_string "OSC8 wraps core text but not padding or border"
    "┌───┐\n│   │\n│ \x1b]8;;https://example.test\x07x\x1b]8;;\x07 │\n│   │\n└───┘"
    (Style.render styled "x");
  check_string "hyperlinks are not inherited" "x"
    (Style.render
       (Style.inherit_ ~parent:(Style.hyperlink link Style.empty) Style.empty)
       "x");
  check_string "unset_hyperlink removes OSC8" "x"
    (Style.render (Style.unset_hyperlink (Style.hyperlink link Style.empty)) "x")

let test_color_props_set_unset () =
  let s = Style.foreground red Style.empty in
  check_color_opt "foreground set" (Some red) (Style.get_foreground s);
  check_color_opt "foreground unset" None
    (Style.get_foreground (Style.unset_foreground s));
  let s = Style.background red Style.empty in
  check_color_opt "background set" (Some red) (Style.get_background s);
  check_color_opt "background unset" None
    (Style.get_background (Style.unset_background s));
  let s = Style.underline true (Style.underline_color red Style.empty) in
  check_color_opt "underline_color set" (Some red) (Style.get_underline_color s);
  check_color_opt "underline_color unset" None
    (Style.get_underline_color (Style.unset_underline_color s));
  let s = Style.margin_background red Style.empty in
  check_color_opt "margin_background set" (Some red) (Style.get_margin_background s);
  check_color_opt "margin_background unset" None
    (Style.get_margin_background (Style.unset_margin_background s))

let test_margin_padding_whole_record_set_unset () =
  let sides = Sides.v ~top:1 ~right:2 ~bottom:3 ~left:4 () in
  let s = Style.margin sides Style.empty in
  check_bool "margin set" true (Style.get_margin s = Some sides);
  check_bool "margin unset clears the whole record" true
    (Style.get_margin (Style.unset_margin s) = None);
  let s = Style.padding sides Style.empty in
  check_bool "padding set" true (Style.get_padding s = Some sides);
  check_bool "padding unset clears the whole record" true
    (Style.get_padding (Style.unset_padding s) = None)

let test_border_sides_set_unset () =
  let s = Style.border Border.normal Style.empty in
  check_bool_opt "border sets all four sides true" (Some true) (Style.get_border_top s);
  check_bool_opt "border_right true" (Some true) (Style.get_border_right s);
  check_bool_opt "border_bottom true" (Some true) (Style.get_border_bottom s);
  check_bool_opt "border_left true" (Some true) (Style.get_border_left s);
  let s = Style.unset_border_top s in
  check_bool_opt "unset_border_top only" None (Style.get_border_top s);
  check_bool_opt "border_right survives unset_border_top" (Some true)
    (Style.get_border_right s);
  let s = Style.unset_border_right s in
  check_bool_opt "unset_border_right" None (Style.get_border_right s);
  let s = Style.unset_border_bottom s in
  check_bool_opt "unset_border_bottom" None (Style.get_border_bottom s);
  let s = Style.unset_border_left s in
  check_bool_opt "unset_border_left" None (Style.get_border_left s);
  (* unset_border clears the border value itself and every side flag. *)
  let s = Style.border Border.normal Style.empty in
  let s = Style.unset_border s in
  check_bool "unset_border clears border" true (Style.get_border s = None);
  check_bool_opt "unset_border clears border_top too" None (Style.get_border_top s);
  let foregrounds = Sides_color.v ~top:red () in
  let s = Style.border_foreground foregrounds (Style.border Border.normal Style.empty) in
  check_bool "unset_border preserves independent border colors" true
    (Style.get_border_foreground (Style.unset_border s) = Some foregrounds)

let test_tab_width_set_unset () =
  let s = Style.tab_width 2 Style.empty in
  check_int_opt "tab_width set" (Some 2) (Style.get_tab_width s);
  check_int_opt "tab_width clamps values below -1" (Some (-1))
    (Style.get_tab_width (Style.tab_width (-2) Style.empty));
  (* Ported behavior, not upstream's literal assertion: upstream's own
     TestStyleUnset checks `GetTabWidth() != 4` after unset, which only
     passes because GetTabWidth's doc claims a 4 default that its actual
     getAsInt(tabWidthKey) call (no default arg) never applies -- an
     upstream doc/implementation mismatch. Our option-based getter sidesteps
     the whole ambiguity: unset is unambiguously None. *)
  check_int_opt "tab_width unset is None, not a default" None
    (Style.get_tab_width (Style.unset_tab_width s))

(* ---------------------------------------------------------------------- *)
(* Inheritance: ports TestStyleInherit (style_test.go:156-192) plus the
   margin_background-from-background fallback documented at style.mli:82-84. *)
let test_inherit_copies_attributes () =
  let parent =
    Style.empty |> Style.bold true |> Style.italic true |> Style.underline true
    |> Style.strikethrough true |> Style.blink true |> Style.faint true
    |> Style.foreground white |> Style.background dark_bg
    |> Style.margin (Sides.all 1)
    |> Style.padding (Sides.all 1)
  in
  let i = Style.inherit_ ~parent Style.empty in
  check_bool "bold inherited" true (Style.get_bold i = Style.get_bold parent);
  check_bool "italic inherited" true (Style.get_italic i = Style.get_italic parent);
  check_bool "underline inherited" true
    (Style.get_underline i = Style.get_underline parent);
  check_bool "strikethrough inherited" true
    (Style.get_strikethrough i = Style.get_strikethrough parent);
  check_bool "blink inherited" true (Style.get_blink i = Style.get_blink parent);
  check_bool "faint inherited" true (Style.get_faint i = Style.get_faint parent);
  check_bool "foreground inherited" true
    (Style.get_foreground i = Style.get_foreground parent);
  check_bool "background inherited" true
    (Style.get_background i = Style.get_background parent);
  check_bool "padding is never inherited" true (Style.get_padding i = None);
  check_bool "margin is never inherited" true (Style.get_margin i = None)

let test_inherit_child_overrides_parent () =
  let parent = Style.bold true Style.empty in
  let child = Style.bold false Style.empty in
  let merged = Style.inherit_ ~parent child in
  check_bool_opt "child's own explicit value wins over parent" (Some false)
    (Style.get_bold merged)

let test_inherit_margin_background_fallback () =
  (* Case A: parent sets only background -> child's margin_background
     falls back to the parent's background. *)
  let parent = Style.background red Style.empty in
  let child = Style.empty in
  check_color_opt "margin_background falls back to parent background" (Some red)
    (Style.get_margin_background (Style.inherit_ ~parent child));
  (* Case B: parent sets both background and margin_background -> the
     parent's own margin_background wins, not its background. *)
  let parent = Style.margin_background blue (Style.background red Style.empty) in
  check_color_opt "explicit parent margin_background beats parent background" (Some blue)
    (Style.get_margin_background (Style.inherit_ ~parent Style.empty));
  (* Case C: the child's own margin_background is never overridden. *)
  let child = Style.margin_background white Style.empty in
  check_color_opt "child's own margin_background survives inheritance" (Some white)
    (Style.get_margin_background (Style.inherit_ ~parent child));
  (* Case D: neither side sets anything -> stays unset. *)
  check_color_opt "no fallback when neither side has a color" None
    (Style.get_margin_background (Style.inherit_ ~parent:Style.empty Style.empty))

(* ---------------------------------------------------------------------- *)
(* Underline/strikethrough family: ports TestGetUnderlineColor
   (style_test.go:54-62), TestUnderline (style_test.go:10-52) and
   TestStrikethrough (style_test.go:64-98).

   Strikethrough and the underline_spaces-only case (upstream test 4) are
   byte-for-byte identical to upstream and are reused verbatim (hand-traced
   through Style.render/ansi_style/apply_ansi and cross-checked against
   test/ansi/test_style.ml's own SGR-order assertions).

   Upstream's TestUnderline cases 5-6 (UnderlineStyle(Curly), optionally
   with UnderlineColor) are not ported: see the note inside
   test_underline_family for why. Of the remaining cases, two set the raw
   `underline` bool, which is where upstream's OWN SGR encoder
   double-emits the underline attribute (once as a bare "4", again as the
   specific style code, e.g. upstream's expected "\x1b[4;4m" for Single).
   Our Charm_ansi.Style.to_sgr emits the attribute once, in the fixed order
   bold,faint,italic,blink,reverse,conceal,strike,underline,fg,bg,
   underline_color (lib/ansi/style.ml:101-116), so the exact bytes diverge
   from upstream's literals by construction, not by behavior: every glyph
   that upstream marks underlined is underlined in our output too. Those
   two cases below use our own hand-derived bytes instead of upstream's
   literals, justified by that trace. *)
let test_get_underline_color () =
  let s = Style.underline true (Style.underline_color red Style.empty) in
  check_color_opt "GetUnderlineColor" (Some red) (Style.get_underline_color s)

let test_underline_family () =
  (* Case: UnderlineSpaces(true) alone (no bare underline flag) -- no
     upstream quirk applies here since the raw `underline` bool is never
     set; byte-identical to upstream's expected "ab\x1b[4m \x1b[mc". *)
  check_string "underline_spaces alone styles only the space" "ab\x1b[4m \x1b[mc"
    (Style.render (Style.underline_spaces true Style.empty) "ab c");
  (* Case: Underline(true) alone == Underline(true) + UnderlineSpaces(true):
     default underline_spaces already tracks underline when unset. *)
  let plain_underline = Style.render (Style.underline true Style.empty) "ab c" in
  let explicit_spaces =
    Style.render (Style.underline_spaces true (Style.underline true Style.empty)) "ab c"
  in
  check_string "explicit underline_spaces(true) matches the implicit default"
    plain_underline explicit_spaces;
  check_string "underline(true) alone (derived; upstream doubles the SGR '4')"
    "\x1b[4ma\x1b[m\x1b[4mb\x1b[m\x1b[4m \x1b[m\x1b[4mc\x1b[m" plain_underline;
  (* Case: Underline(true) + UnderlineSpaces(false) -- the space is left
     completely bare (te_space collapses to Charm_ansi.Style.default). *)
  check_string "underline(true).underline_spaces(false) leaves the space bare"
    "\x1b[4ma\x1b[m\x1b[4mb\x1b[m \x1b[4mc\x1b[m"
    (Style.render
       (Style.underline_spaces false (Style.underline true Style.empty))
       "ab c")
(* Note: UnderlineStyle(Curly) cases (upstream tests 5-6) are not ported
     here. Style.underline_style requires a Charm_ansi.Style.underline
     value as its argument (style.mli), and Charm_lipgloss re-exports only
     Charm_ansi.Color, not Charm_ansi.Style; this test file has no access
     to that module without adding charm.ansi as an explicit dependency of
     test/lipgloss's dune stanza, which is outside this file-only scope
     (see report). *)

let test_strikethrough_family () =
  check_string "strikethrough(true) alone"
    "\x1b[9ma\x1b[m\x1b[9mb\x1b[m\x1b[9m \x1b[m\x1b[9mc\x1b[m"
    (Style.render (Style.strikethrough true Style.empty) "ab c");
  check_string "strikethrough(true).strikethrough_spaces(true) same as default"
    "\x1b[9ma\x1b[m\x1b[9mb\x1b[m\x1b[9m \x1b[m\x1b[9mc\x1b[m"
    (Style.render
       (Style.strikethrough_spaces true (Style.strikethrough true Style.empty))
       "ab c");
  check_string "strikethrough(true).strikethrough_spaces(false) leaves the space bare"
    "\x1b[9ma\x1b[m\x1b[9mb\x1b[m \x1b[9mc\x1b[m"
    (Style.render
       (Style.strikethrough_spaces false (Style.strikethrough true Style.empty))
       "ab c");
  check_string "strikethrough_spaces(true) alone styles only the space"
    "ab\x1b[9m \x1b[mc"
    (Style.render (Style.strikethrough_spaces true Style.empty) "ab c")

(* ---------------------------------------------------------------------- *)
(* Plain flag rendering: ports TestStyleRender (style_test.go:100-142). The
   non-underline sub-cases carry no SGR-minimization divergence and are
   reused byte for byte. The underline sub-case is re-derived per the
   comment above test_underline_family. *)
let test_style_render_flags () =
  check_string "foreground" "\x1b[38;2;90;86;224mhello\x1b[m"
    (Style.render
       (Style.foreground (Option.get (Color.rgb 90 86 224)) Style.empty)
       "hello");
  check_string "bold" "\x1b[1mhello\x1b[m"
    (Style.render (Style.bold true Style.empty) "hello");
  check_string "italic" "\x1b[3mhello\x1b[m"
    (Style.render (Style.italic true Style.empty) "hello");
  check_string "underline (derived; no spaces present, upstream still doubles the '4')"
    "\x1b[4mh\x1b[m\x1b[4me\x1b[m\x1b[4ml\x1b[m\x1b[4ml\x1b[m\x1b[4mo\x1b[m"
    (Style.render (Style.underline true Style.empty) "hello");
  check_string "blink" "\x1b[5mhello\x1b[m"
    (Style.render (Style.blink true Style.empty) "hello");
  check_string "faint" "\x1b[2mhello\x1b[m"
    (Style.render (Style.faint true Style.empty) "hello")

(* ---------------------------------------------------------------------- *)
(* Transform, tabs, CRLF, and inline newline stripping: ports
   TestStringTransform (style_test.go:449-492), TestTabConversion
   (style_test.go:438-447), TestCarriageReturnInRender
   (style_test.go:520-531), plus an inline+border interaction case proving
   border/margin/padding are all skipped in inline mode. *)
let test_string_transform () =
  let wrap s = "\x1b[1m" ^ s ^ "\x1b[m" in
  let style = Style.bold true Style.empty in
  check_string "no-op transform" (wrap "hello")
    (Style.render (Style.transform (fun s -> s) style) "hello");
  check_string "uppercase transform" (wrap "RAOW")
    (Style.render (Style.transform String.uppercase_ascii style) "raow");
  let reverse_utf8 s =
    let b = Buffer.create (String.length s) in
    let chars = Stdlib.List.init (String.length s) (fun i -> s.[i]) in
    Stdlib.List.iter (Buffer.add_char b) (Stdlib.List.rev chars);
    Buffer.contents b
  in
  (* ASCII-only reversal check (byte reversal is safe here: no multi-byte
     UTF-8 sequence appears in this input, unlike upstream's Chinese
     example, which needs full Unicode-aware reversal we don't replicate
     byte-for-byte here). *)
  check_string "reverse transform" (wrap "olleh")
    (Style.render (Style.transform reverse_utf8 style) "hello")

let test_tab_conversion () =
  check_string "default tab_width is 4" "[    ]" (Style.render Style.empty "[\t]");
  check_string "tab_width 2" "[  ]" (Style.render (Style.tab_width 2 Style.empty) "[\t]");
  check_string "tab_width 0 removes tabs" "[]"
    (Style.render (Style.tab_width 0 Style.empty) "[\t]");
  check_string "tab_width -1 keeps the literal tab" "[\t]"
    (Style.render (Style.tab_width (-1) Style.empty) "[\t]")

let test_carriage_return_normalized () =
  let style = Style.margin (Sides.v ~left:1 ()) Style.empty in
  let with_crlf =
    Style.render style "Super duper california oranges\r\nHello world\r\n"
  in
  let with_lf = Style.render style "Super duper california oranges\nHello world\n" in
  check_string "\\r\\n normalizes to the same render as \\n" with_lf with_crlf

let test_inline_suppresses_border_padding_margin () =
  let style =
    Style.empty |> Style.inline true |> Style.border Border.rounded
    |> Style.padding (Sides.all 2)
    |> Style.margin (Sides.all 2)
  in
  check_string "inline mode flattens newlines and skips border/padding/margin entirely"
    "xy" (Style.render style "x\ny")

(* ---------------------------------------------------------------------- *)
(* Exact width vs minimum height: ports TestWidth/TestHeight
   (style_test.go:533-581) using Layout.width/height and hand-computed
   frame sizes (Style exposes no GetHorizontalFrameSize/GetVerticalFrameSize
   convenience getters -- see report), across the same four border variants
   upstream tests. *)
let long_content =
  "The Romans learned from the Greeks that quinces slowly cooked with honey would \
   \xe2\x80\x9cset\xe2\x80\x9d when cool. The Apicius gives a recipe for preserving \
   whole quinces, stems and leaves attached, in a bath of honey diluted with defrutum: \
   Roman marmalade. Preserves of quince and lemon appear (along with rose, apple, plum \
   and pear) in the Book of ceremonies of the Byzantine Emperor Constantine VII \
   Porphyrogennetos."

let test_width_exact_across_border_variants () =
  let check name style horizontal_frame_size =
    let content_width = 80 - horizontal_frame_size in
    let rendered = Style.render (Style.width content_width style) long_content in
    check_int name content_width (Layout.width rendered)
  in
  check "width with borders"
    (Style.padding (Sides.v ~right:2 ~left:2 ()) (Style.border Border.normal Style.empty))
    6;
  check "width no borders" (Style.padding (Sides.v ~right:2 ~left:2 ()) Style.empty) 4;
  check "width unset borders"
    (Style.border_right false
       (Style.border_left false
          (Style.padding (Sides.v ~right:2 ~left:2 ())
             (Style.border Border.normal Style.empty))))
    4;
  check "width single-sided border (left only)"
    (Style.border_right false
       (Style.border_bottom false
          (Style.border_top false
             (Style.padding (Sides.v ~right:2 ~left:2 ())
                (Style.border Border.normal Style.empty)))))
    5

let test_height_minimum_across_border_variants () =
  let check name style vertical_frame_size =
    let content_height = 20 - vertical_frame_size in
    let rendered =
      Style.render (Style.height content_height (Style.width 80 style)) long_content
    in
    check_int name content_height (Layout.height rendered)
  in
  check "height with borders"
    (Style.padding (Sides.v ~right:2 ~left:2 ()) (Style.border Border.normal Style.empty))
    2;
  check "height no borders" (Style.padding (Sides.v ~right:2 ~left:2 ()) Style.empty) 0;
  check "height unset borders"
    (Style.border_bottom false
       (Style.border_top false
          (Style.padding (Sides.v ~right:2 ~left:2 ())
             (Style.border Border.normal Style.empty))))
    0;
  check "height single-sided border (top only)"
    (Style.border_left false
       (Style.border_right false
          (Style.border_bottom false
             (Style.padding (Sides.v ~right:2 ~left:2 ())
                (Style.border Border.normal Style.empty)))))
    1

let test_height_never_truncates_but_pads_up () =
  let five_lines = "a\nb\nc\nd\ne" in
  check_int "height smaller than content leaves every line" 5
    (Layout.height (Style.render (Style.height 2 Style.empty) five_lines));
  check_int "height larger than content pads up to the minimum" 4
    (Layout.height (Style.render (Style.height 4 Style.empty) "a\nb"))

(* ---------------------------------------------------------------------- *)
(* Max clipping: no applicable upstream style_test.go cases exist for
   MaxWidth/MaxHeight (only present in an untestable benchmark), so these
   are derived directly from the render order documented in style.mli and
   the plan: max_width/max_height apply AFTER border and margins. *)
let test_max_width_truncates_plain_text () =
  check_string "max_width truncates a single line" "hello"
    (Style.render (Style.max_width 5 Style.empty) "hello world")

let test_max_height_keeps_first_lines () =
  check_string "max_height keeps only the first N lines" "a\nb"
    (Style.render (Style.max_height 2 Style.empty) "a\nb\nc\nd\ne")

let test_max_width_truncates_after_border () =
  (* max_width applies to the fully bordered line, cutting into the border
     glyphs themselves -- proving the documented "border then margins ...
     then max_width" render order (border_h=2 here, content "hello" is
     never wrapped since Style.width is unset). *)
  check_string "max_width=2 truncates into the border glyphs"
    "\xe2\x94\x8c\xe2\x94\x80\n\xe2\x94\x82h\n\xe2\x94\x94\xe2\x94\x80"
    (Style.render (Style.max_width 2 (Style.border Border.normal Style.empty)) "hello")

(* ---------------------------------------------------------------------- *)
(* Render order (padding inside border inside margin): a fully hand-derived,
   color-free structural test. No applicable upstream literal exists (this
   combination isn't in any upstream test), so every byte below is traced
   through Style.render step by step: content "x" (1x1) -> padding(1) makes
   a 3x3 block -> Border.rounded wraps it in a 5x5 box -> margin(1) adds one
   blank row/col on every side for a final 7x7 block. *)
let test_render_order_padding_border_margin () =
  let style =
    Style.empty
    |> Style.padding (Sides.all 1)
    |> Style.border Border.rounded
    |> Style.margin (Sides.all 1)
  in
  let rendered = Style.render style "x" in
  check_string "padding sits inside the border, border sits inside the margin"
    "       \n\
    \ \xe2\x95\xad\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x95\xae \n\
    \ \xe2\x94\x82   \xe2\x94\x82 \n\
    \ \xe2\x94\x82 x \xe2\x94\x82 \n\
    \ \xe2\x94\x82   \xe2\x94\x82 \n\
    \ \xe2\x95\xb0\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x95\xaf \n\
    \       "
    rendered;
  check_int "outermost width is content(1) + padding(2) + border(2) + margin(2)" 7
    (Layout.width rendered);
  check_int "outermost height is content(1) + padding(2) + border(2) + margin(2)" 7
    (Layout.height rendered)

(* ---------------------------------------------------------------------- *)
(* Per-edge border colors and multicolumn (wide-glyph) borders: no
   applicable upstream test exists (borders_test.go is 100% benchmarks plus
   one internal-helper test), so these are derived from Sides_color's own
   documented per-edge independence (sides_color.mli) and the "widest-rune
   edge width rule" the plan calls out for step 6. *)
let test_border_per_edge_colors () =
  let style =
    Style.border Border.normal Style.empty
    |> Style.border_foreground (Sides_color.v ~top:red ())
  in
  let rendered = Style.render style "x" in
  check_string "only the top edge is colored; left/right/bottom stay plain"
    "\x1b[38;2;255;0;0m\xe2\x94\x8c\xe2\x94\x80\xe2\x94\x90\x1b[m\n\
     \xe2\x94\x82x\xe2\x94\x82\n\
     \xe2\x94\x94\xe2\x94\x80\xe2\x94\x98"
    rendered;
  let foregrounds = Sides_color.v ~top:red () in
  let backgrounds = Sides_color.v ~bottom:blue () in
  check_bool "border_foreground getter preserves per-edge values" true
    (Style.get_border_foreground (Style.border_foreground foregrounds Style.empty)
    = Some foregrounds);
  check_bool "border_background getter preserves per-edge values" true
    (Style.get_border_background (Style.border_background backgrounds Style.empty)
    = Some backgrounds);
  check_bool "unset border_foreground clears the whole record" true
    (Style.get_border_foreground
       (Style.unset_border_foreground (Style.border_foreground foregrounds Style.empty))
    = None);
  check_bool "unset border_background clears the whole record" true
    (Style.get_border_background
       (Style.unset_border_background (Style.border_background backgrounds Style.empty))
    = None)

let test_border_wide_corner_consistency () =
  (* A public Border.t may use a wide corner glyph. Edge slots must retain the
     widest-rune width so horizontal and body rows have identical geometry. *)
  let wide_left_border = { Border.normal with top_left = "\xe4\xb8\x96" } in
  let rendered = Style.render (Style.border wide_left_border Style.empty) "x" in
  match String.split_on_char '\n' rendered with
  | [ top; body; bottom ] ->
      check_int "top row width matches the body row width" (Layout.width body)
        (Layout.width top);
      check_int "bottom row width matches the body row width" (Layout.width body)
        (Layout.width bottom)
  | _ -> Alcotest.fail "expected exactly three rendered lines (top, body, bottom)"

(* ---------------------------------------------------------------------- *)
(* color_whitespace: derived directly from style.go's own render function
   (colorWhitespace/styleWhitespace gating, .references/lipgloss/style.go
   around line 305 and 363-380), which our te_whitespace construction
   (style.ml) was hand-verified against term by term. *)
let test_color_whitespace () =
  let style = Style.padding (Sides.all 1) (Style.background red Style.empty) in
  let colored = Style.render style "x" in
  (* The background always colors "x" itself (the plain content style);
     color_whitespace gates only whether the PADDING cells around it also
     get that background -- so even the default-true case wraps "x" in its
     own separate SGR run rather than sharing one run with the padding. *)
  check_string "color_whitespace defaults to true: padding gets the background too"
    "\x1b[48;2;255;0;0m   \x1b[m\n\
     \x1b[48;2;255;0;0m \x1b[m\x1b[48;2;255;0;0mx\x1b[m\x1b[48;2;255;0;0m \x1b[m\n\
     \x1b[48;2;255;0;0m   \x1b[m"
    colored;
  let uncolored = Style.render (Style.color_whitespace false style) "x" in
  check_string "color_whitespace(false) leaves the padding bare but \"x\" stays colored"
    "   \n \x1b[48;2;255;0;0mx\x1b[m \n   " uncolored;
  (* Reverse's foreground-on-whitespace rule (styleWhitespace = reverse in
     upstream) is independent of color_whitespace: it still applies to the
     padding even when color_whitespace is explicitly disabled. *)
  let reverse_style = Style.color_whitespace false (Style.reverse true style) in
  let reverse_rendered = Style.render reverse_style "x" in
  check_bool "reverse's fg-on-padding rule ignores color_whitespace" true
    (reverse_rendered <> uncolored)

(* ---------------------------------------------------------------------- *)
(* Margin-only rendering with no other property set: ports the four
   MarginLeft/MarginRight cases of TestStyleValue (style_test.go:364-431).
   The SetString-based cases from that table are dropped: Style holds no
   text of its own by design (render takes the text as a plain argument),
   so "set string" / "new style with string" have no OCaml counterpart. *)
let test_margin_only_render () =
  check_string "margin right on non-empty text" "foo "
    (Style.render (Style.margin (Sides.v ~right:1 ()) Style.empty) "foo");
  check_string "margin left on non-empty text" " foo"
    (Style.render (Style.margin (Sides.v ~left:1 ()) Style.empty) "foo");
  check_string "margin right on empty text" " "
    (Style.render (Style.margin (Sides.v ~right:1 ()) Style.empty) "");
  check_string "margin left on empty text" " "
    (Style.render (Style.margin (Sides.v ~left:1 ()) Style.empty) "")

let test_escape_payload_terminators () =
  let style = Style.underline true Style.empty in
  let styled text = "\x1b[4m" ^ text ^ "\x1b[m" in
  let terminators = [ "\x07"; "\x1b\\"; "\x9c"; "\x18"; "\x1a" ] in
  Stdlib.List.iteri
    (fun index terminator ->
      let payload = "\x1b]8;;hidden payload " ^ terminator in
      check_string
        ("escape payload terminator " ^ string_of_int index)
        (styled "a" ^ payload ^ styled "b")
        (Style.render style ("a" ^ payload ^ "b")))
    terminators

let test_zero_width_border_glyphs () =
  let combining = "\xcc\x81" in
  let border =
    {
      Border.top = combining;
      bottom = combining;
      left = combining;
      right = combining;
      top_left = combining;
      top_right = combining;
      bottom_left = combining;
      bottom_right = combining;
      middle_left = combining;
      middle_right = combining;
      middle = combining;
      middle_top = combining;
      middle_bottom = combining;
    }
  in
  let rendered = Style.render (Style.border border Style.empty) "x" in
  check_int "zero-width border glyphs do not create fake cells" 1 (Layout.width rendered)

let cases =
  [
    ("bool props set/unset", `Quick, test_bool_props_set_unset);
    ("hyperlink render and inheritance", `Quick, test_hyperlink_render_and_inherit);
    ("color props set/unset", `Quick, test_color_props_set_unset);
    ( "margin/padding whole-record set/unset",
      `Quick,
      test_margin_padding_whole_record_set_unset );
    ("border sides independent set/unset", `Quick, test_border_sides_set_unset);
    ("tab_width set/unset", `Quick, test_tab_width_set_unset);
    ( "inherit copies attributes, never padding/margin",
      `Quick,
      test_inherit_copies_attributes );
    ("inherit: child overrides parent", `Quick, test_inherit_child_overrides_parent);
    ( "inherit: margin_background fallback chain",
      `Quick,
      test_inherit_margin_background_fallback );
    ("get_underline_color", `Quick, test_get_underline_color);
    ("underline family", `Quick, test_underline_family);
    ("strikethrough family", `Quick, test_strikethrough_family);
    ("style render flags", `Quick, test_style_render_flags);
    ("string transform", `Quick, test_string_transform);
    ("tab conversion", `Quick, test_tab_conversion);
    ("carriage return normalized", `Quick, test_carriage_return_normalized);
    ( "inline suppresses border/padding/margin",
      `Quick,
      test_inline_suppresses_border_padding_margin );
    ("width exact across border variants", `Quick, test_width_exact_across_border_variants);
    ( "height minimum across border variants",
      `Quick,
      test_height_minimum_across_border_variants );
    ( "height never truncates, pads up to minimum",
      `Quick,
      test_height_never_truncates_but_pads_up );
    ("max_width truncates plain text", `Quick, test_max_width_truncates_plain_text);
    ("max_height keeps first lines", `Quick, test_max_height_keeps_first_lines);
    ("max_width truncates after border", `Quick, test_max_width_truncates_after_border);
    ( "render order: padding, border, margin",
      `Quick,
      test_render_order_padding_border_margin );
    ("border per-edge colors", `Quick, test_border_per_edge_colors);
    ("border wide-corner width consistency", `Quick, test_border_wide_corner_consistency);
    ("zero-width border glyphs", `Quick, test_zero_width_border_glyphs);
    ("escape payload terminators", `Quick, test_escape_payload_terminators);
    ("color_whitespace", `Quick, test_color_whitespace);
    ("margin-only render", `Quick, test_margin_only_render);
  ]
