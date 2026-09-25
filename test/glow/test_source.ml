let classify_pipe () =
  match Source.classify ~argument:None ~cwd:"/tmp" ~stdin_is_tty:false with
  | Ok Source.Stdin -> ()
  | _ -> Alcotest.fail "a pipe should select stdin"

let classify_directory () =
  match Source.classify ~argument:None ~cwd:"/tmp" ~stdin_is_tty:true with
  | Ok (Source.Directory "/tmp") -> ()
  | _ -> Alcotest.fail "a tty should select the current directory"

let discover_hidden () =
  Test_support.with_temp_dir (fun dir ->
      let root = Eio.Path.native_exn dir in
      let visible = Filename.concat root "README.md" in
      let hidden_dir = Filename.concat root ".hidden" in
      let ignored_dir = Filename.concat root "node_modules" in
      Unix.mkdir hidden_dir 0o700;
      Unix.mkdir ignored_dir 0o700;
      let hidden = Filename.concat hidden_dir "secret.md" in
      let ignored = Filename.concat ignored_dir "ignored.md" in
      let channel = open_out visible in
      output_string channel "# visible";
      close_out channel;
      let channel = open_out hidden in
      output_string channel "# hidden";
      close_out channel;
      let channel = open_out ignored in
      output_string channel "# ignored";
      close_out channel;
      let normal = Source.discover_markdown ~root ~show_hidden:false in
      Alcotest.(check (list string)) "normal" [ visible ] normal;
      let all = Source.discover_markdown ~root ~show_hidden:true in
      Alcotest.(check int) "all count" 2 (List.length all))

let readme_urls () =
  let urls = Source.readme_candidates ~host:"github.com" ~owner:"owner" ~repo:"repo" in
  Alcotest.(check bool)
    "raw URL" true
    (List.exists
       (String.equal "https://raw.githubusercontent.com/owner/repo/HEAD/README.md")
       urls)

let url_classification () =
  match
    Source.classify ~argument:(Some "https://example.com/readme.md") ~cwd:"/tmp"
      ~stdin_is_tty:true
  with
  | Ok (Source.Url url) ->
      Alcotest.(check string) "url" "https://example.com/readme.md" url
  | _ -> Alcotest.fail "HTTP URL should be classified as a URL"

let unsupported_scheme () =
  match
    Source.classify ~argument:(Some "ftp://example.com/readme.md") ~cwd:"/tmp"
      ~stdin_is_tty:true
  with
  | Error (`Invalid _) -> ()
  | _ -> Alcotest.fail "unsupported schemes should fail classification"

let frontmatter () =
  let input = "---\ntitle: Example\n---\n# Heading\n" in
  Alcotest.(check string) "frontmatter" "# Heading\n" (Source.remove_frontmatter input)

let suite =
  ( "source",
    [
      Alcotest.test_case "pipe" `Quick classify_pipe;
      Alcotest.test_case "directory" `Quick classify_directory;
      Alcotest.test_case "discovery" `Quick discover_hidden;
      Alcotest.test_case "readme URLs" `Quick readme_urls;
      Alcotest.test_case "url" `Quick url_classification;
      Alcotest.test_case "unsupported scheme" `Quick unsupported_scheme;
      Alcotest.test_case "frontmatter" `Quick frontmatter;
    ] )
