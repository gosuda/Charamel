let plain s =
  let out = Buffer.create (String.length s) in
  String.split_on_char '\n' (Charm_ansi.Text.strip s)
  |> List.iteri (fun i line ->
      if i > 0 then Buffer.add_char out '\n';
      Buffer.add_string out (String.trim line));
  Buffer.contents out

let contains haystack needle =
  let h = String.length haystack and n = String.length needle in
  let rec loop i =
    if i + n > h then false
    else if String.sub haystack i n = needle then true
    else loop (i + 1)
  in
  if n = 0 then true else loop 0

let test_empty () = Alcotest.(check string) "empty markdown" "" (Charm_glamour.render "")

let test_heading_and_inline_nodes () =
  let output = Charm_glamour.render "# Hello *world* **again** ~~old~~ `code`" |> plain in
  Alcotest.(check bool) "heading text" true (contains output "Hello");
  Alcotest.(check bool) "emphasis text" true (contains output "world");
  Alcotest.(check bool) "strong text" true (contains output "again");
  Alcotest.(check bool) "strike text" true (contains output "old");
  Alcotest.(check bool) "code text" true (contains output "code")

let test_width_and_no_wrap () =
  let source = "one two three four five six seven" in
  let wrapped = Charm_glamour.render ~width:12 source |> plain in
  let lines = String.split_on_char '\n' wrapped in
  Alcotest.(check bool) "wrap produces multiple lines" true (List.length lines > 1);
  let unwrapped = Charm_glamour.render ~width:0 source |> plain in
  Alcotest.(check bool) "zero disables wrapping" true (contains unwrapped source)

let test_preserve_newlines () =
  let source = "first\nsecond" in
  let folded = Charm_glamour.render source |> plain in
  let preserved = Charm_glamour.render ~preserve_newlines:true source |> plain in
  Alcotest.(check bool) "default folds a soft break" true (contains folded "first second");
  Alcotest.(check bool)
    "preserve keeps a soft break" true
    (contains preserved "first\nsecond")

let test_emoji () =
  let output = Charm_glamour.render ~emoji:true ":smile: :heart:" |> plain in
  Alcotest.(check bool) "github shortcode smile" true (contains output "😄");
  Alcotest.(check bool) "github shortcode heart" true (contains output "❤")

let test_lists_and_tasks () =
  let markdown = "- one\n  - nested\n- [x] done\n- [ ] todo" in
  let output = Charm_glamour.render markdown |> plain in
  Alcotest.(check bool) "nested item" true (contains output "nested");
  Alcotest.(check bool) "checked task" true (contains output "[✓]");
  Alcotest.(check bool) "unchecked task" true (contains output "[ ]")

let test_links_and_table_footer () =
  let markdown = "| Name | Link |\n| --- | --- |\n| Charm | [site](https://charm.sh) |" in
  let output = Charm_glamour.render markdown |> plain in
  Alcotest.(check bool) "table content" true (contains output "Charm");
  Alcotest.(check bool)
    "table link footer" true
    (contains output "[1]: site https://charm.sh")

let test_table_alignment_and_truncation () =
  let markdown =
    "| Left | Centre | Right |\n\
     | :--- | :----: | ---: |\n\
     | a very long value | middle | another very long value |"
  in
  let output = Charm_glamour.render ~width:36 ~table_wrap:false markdown |> plain in
  Alcotest.(check bool) "table truncates with ellipsis" true (contains output "…");
  Alcotest.(check bool) "table keeps all columns" true (contains output "Centre")

let test_relative_url () =
  let output =
    Charm_glamour.render ~base_url:"https://example.com/docs/" "[guide](guide.md)"
  in
  Alcotest.(check bool)
    "resolved href" true
    (contains output "https://example.com/docs/guide.md")

let test_code_block_highlighting () =
  let output = Charm_glamour.render "```ocaml\nlet x = 1\n```" in
  Alcotest.(check bool) "code text survives" true (contains (plain output) "let x = 1");
  Alcotest.(check bool) "known lexer styles" true (contains output "\027[")

let test_footnotes_and_html () =
  let markdown = "See note[^1].\n\n[^1]: Footnote text\n\n<div>raw</div>" in
  let output = Charm_glamour.render markdown |> plain in
  Alcotest.(check bool) "footnote reference" true (contains output "[1]");
  Alcotest.(check bool) "footnote body" true (contains output "Footnote text");
  Alcotest.(check bool) "html block" true (contains output "<div>raw</div>")

let test_example_golden_scenarios () =
  let source = In_channel.with_open_bin "data/example.md" In_channel.input_all in
  let output_80 = Charm_glamour.render ~width:80 source |> plain in
  let output_120 = Charm_glamour.render ~width:120 source |> plain in
  Alcotest.(check bool) "80-column example" true (contains output_80 "Glamour");
  Alcotest.(check bool) "120-column example" true (contains output_120 "artichoke");
  Alcotest.(check bool) "width changes layout" true (output_80 <> output_120)

let fixture_files directory =
  Sys.readdir directory |> Array.to_list |> List.sort String.compare

let test_upstream_regressions () =
  let directories = [ "data/examples"; "data/issues" ] in
  List.iter
    (fun directory ->
      fixture_files directory
      |> List.filter (fun file -> Filename.check_suffix file ".md")
      |> List.iter (fun file ->
          let path = Filename.concat directory file in
          let source = In_channel.with_open_bin path In_channel.input_all in
          let output = Charm_glamour.render source in
          (* Source fixtures copied from Charmbracelet/glamour v2.0.1,
                styles/examples and testdata/issues; upstream is MIT.
                These are smoke regressions only; this port does not claim
                unproven byte-for-byte parity. *)
          Alcotest.(check bool)
            ("rendered " ^ path) true
            (String.length (plain output) > 0)))
    directories

let () =
  Alcotest.run "charm.glamour"
    [
      ( "renderer",
        [
          Alcotest.test_case "empty" `Quick test_empty;
          Alcotest.test_case "heading and inline" `Quick test_heading_and_inline_nodes;
          Alcotest.test_case "width" `Quick test_width_and_no_wrap;
          Alcotest.test_case "preserve newlines" `Quick test_preserve_newlines;
          Alcotest.test_case "emoji" `Quick test_emoji;
          Alcotest.test_case "lists and tasks" `Quick test_lists_and_tasks;
          Alcotest.test_case "links and table footer" `Quick test_links_and_table_footer;
          Alcotest.test_case "table alignment and truncation" `Quick
            test_table_alignment_and_truncation;
          Alcotest.test_case "relative URL" `Quick test_relative_url;
          Alcotest.test_case "code block" `Quick test_code_block_highlighting;
          Alcotest.test_case "footnotes and html" `Quick test_footnotes_and_html;
          Alcotest.test_case "example goldens" `Quick test_example_golden_scenarios;
          Alcotest.test_case "upstream fixtures" `Quick test_upstream_regressions;
        ] );
    ]
