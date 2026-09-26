module H = Charamel_highlight

let source lines =
  if List.length lines <> 10 then invalid_arg "highlight samples must have ten lines";
  String.concat "\n" lines

let samples =
  [
    ( "ocaml",
      H.find ".ml",
      source
        [
          "let answer = 42";
          "type user = { name : string }";
          "(* comment *)";
          "let greet name = \"hello \" ^ name";
          "if answer > 0 then print_endline \"yes\"";
          "let raw = {|raw|}";
          "[@@@warning \"-27\"]";
          "match Some answer with";
          "| Some n -> n";
          "| None -> 0";
        ],
      [
        H.Keyword;
        H.Number;
        H.Type;
        H.Comment;
        H.String;
        H.Builtin;
        H.Operator;
        H.Attribute;
        H.Constant;
      ] );
    ( "go",
      H.find "go",
      source
        [
          "package main";
          "import \"fmt\"";
          "// comment";
          "type User struct {";
          "  Name string";
          "}";
          "func main() {";
          "  const n = 3";
          "  println(n)";
          "  var missing = nil";
        ],
      [ H.Keyword; H.String; H.Comment; H.Type; H.Constant; H.Number; H.Builtin ] );
    ( "rust",
      H.find ".rs",
      source
        [
          "fn main() {";
          "  let value: Option<i32> = Some(3);";
          "  // comment";
          "  let raw = r#\"raw\"#;";
          "  println!(\"{value}\");";
          "  if value.is_some() {";
          "    return;";
          "  }";
          "}";
          "// done";
        ],
      [ H.Keyword; H.Type; H.Constant; H.Number; H.Comment; H.String; H.Builtin ] );
    ( "python",
      H.find "py",
      source
        [
          "@decorator";
          "def greet(name: str) -> str:";
          "    # comment";
          "    value = 3.14";
          "    return \"hello \" + name";
          "class User:";
          "    active = True";
          "    raw = '''raw text'''";
          "    return None";
          "print(greet(\"world\"))";
        ],
      [
        H.Attribute;
        H.Keyword;
        H.Type;
        H.Comment;
        H.Number;
        H.String;
        H.Constant;
        H.Builtin;
      ] );
    ( "javascript",
      H.find ".js",
      source
        [
          "const answer = 42;";
          "// comment";
          "function greet(name) {";
          "  return `hello ${name}`;";
          "}";
          "if (answer > 0) console.log(true);";
          "class User {}";
          "const object = null;";
          "/* block */";
          "export { greet };";
        ],
      [ H.Keyword; H.Number; H.Comment; H.Builtin; H.String; H.Constant; H.Operator ] );
    ( "typescript",
      H.find "foo.tsx",
      source
        [
          "interface User {";
          "  name: Array<string>;";
          "}";
          "const answer: number = 42;";
          "// comment";
          "function greet(user: User): string {";
          "  return `hi ${user.name}`;";
          "}";
          "console.log(answer);";
          "const missing = undefined;";
        ],
      [ H.Keyword; H.Type; H.Number; H.Comment; H.String; H.Constant; H.Builtin ] );
    ( "json",
      H.find "json",
      source
        [
          "{";
          "  \"name\": \"charm\",";
          "  \"count\": 3,";
          "  \"enabled\": true,";
          "  \"missing\": null,";
          "  \"items\": [";
          "    \"one\",";
          "    false";
          "  ]";
          "}";
        ],
      [ H.String; H.Number; H.Constant; H.Punct ] );
    ( "yaml",
      H.find ".YML",
      source
        [
          "defaults: &defaults";
          "  name: charm";
          "  enabled: true";
          "  count: 3";
          "config: *defaults";
          "value: \"text\"";
          "tag: !!str value";
          "# comment";
          "list:";
          "  - no";
        ],
      [ H.Attribute; H.Ident; H.Constant; H.Number; H.String; H.Comment; H.Operator ] );
    ( "toml",
      H.find "toml",
      source
        [
          "[package]";
          "name = \"charm\"";
          "version = \"1.0\"";
          "description = '''text'''";
          "enabled = true";
          "count = 3";
          "date = 2026-09-16";
          "[dependencies]";
          "# comment";
          "foo.bar = \"baz\"";
        ],
      [ H.Attribute; H.String; H.Constant; H.Number; H.Comment; H.Operator ] );
    ( "bash",
      H.find "zsh",
      source
        [
          "#!/usr/bin/env bash";
          "name=charm";
          "echo \"hello $name\"";
          "if [ \"$name\" = charm ]; then";
          "  printf '%s\\n' \"$name\"";
          "fi";
          "# comment";
          "for item in one two; do";
          "  echo $item";
          "done";
        ],
      [ H.Comment; H.Attribute; H.Builtin; H.String; H.Keyword; H.Operator ] );
    ( "c",
      H.find ".h",
      source
        [
          "#include <stdio.h>";
          "// comment";
          "int main(void) {";
          "  const int value = 3;";
          "  printf(\"%d\\n\", value);";
          "  return 0;";
          "}";
          "/* block */";
          "#define FLAG true";
          "typedef unsigned int uint;";
        ],
      [ H.Keyword; H.Comment; H.Type; H.Constant; H.Number; H.String; H.Builtin ] );
    ( "cpp",
      H.find "foo.hpp",
      source
        [
          "#include <string>";
          "[[nodiscard]]";
          "class User {";
          "public:";
          "  std::string name;";
          "  auto value() -> int { return 3; }";
          "};";
          "// comment";
          "const char* raw = R\"(raw)\";";
          "return nullptr;";
        ],
      [ H.Keyword; H.Attribute; H.Type; H.String; H.Number; H.Comment; H.Constant ] );
    ( "java",
      H.find "java",
      source
        [
          "package example;";
          "import java.util.List;";
          "@Override";
          "public class User {";
          "  private String name;";
          "  public int count = 3;";
          "  // comment";
          "  String text = \"hello\";";
          "  return null;";
          "}";
        ],
      [ H.Keyword; H.Attribute; H.Type; H.Number; H.Comment; H.String; H.Constant ] );
    ( "kotlin",
      H.find ".kt",
      source
        [
          "package example";
          "@JvmStatic";
          "fun greet(name: String): String {";
          "  val count: Int = 3";
          "  val raw = \"\"\"text\"\"\"";
          "  println(raw)";
          "  if (count > 0) return name";
          "  return \"\"";
          "}";
          "// comment";
        ],
      [ H.Keyword; H.Attribute; H.Type; H.Number; H.String; H.Builtin; H.Comment ] );
    ( "swift",
      H.find "swift",
      source
        [
          "import Foundation";
          "@main";
          "struct User {";
          "  let name: String";
          "  let count = 3";
          "  let raw = #\"\"\"text\"\"\"#";
          "  // comment";
          "  print(name)";
          "}";
          "let value: Bool = true";
        ],
      [
        H.Keyword;
        H.Attribute;
        H.Type;
        H.Number;
        H.String;
        H.Comment;
        H.Builtin;
        H.Constant;
      ] );
    ( "ruby",
      H.find ".rb",
      source
        [
          "class User";
          "  @@count = 3";
          "  def initialize(name)";
          "    @name = name";
          "    puts \"hello\"";
          "  end";
          "  words = %w[one two]";
          "  # comment";
          "  true";
          "end";
        ],
      [
        H.Keyword;
        H.Attribute;
        H.Number;
        H.Builtin;
        H.String;
        H.String;
        H.Comment;
        H.Constant;
      ] );
    ( "php",
      H.find "php",
      source
        [
          "<?php";
          "class User {";
          "  public string $name;";
          "  // comment";
          "  public function greet(): string {";
          "    return \"hello \" . $this->name;";
          "  }";
          "  const COUNT = 3;";
          "  /* block */";
          "}";
        ],
      [ H.Keyword; H.Type; H.Attribute; H.Comment; H.String; H.Number; H.Operator ] );
    ( "html",
      H.find ".htm",
      source
        [
          "<!DOCTYPE html>";
          "<!-- comment -->";
          "<html>";
          "<body class=\"page\">";
          "<h1>Hello</h1>";
          "<p>Text &amp; more</p>";
          "</body>";
          "</html>";
          "plain text";
          "&#35;";
        ],
      [ H.Keyword; H.Comment; H.Attribute; H.String; H.Ident ] );
    ( "css",
      H.find "css",
      source
        [
          ".box {";
          "  color: red;";
          "  margin: 10px;";
          "  --custom: \"value\";";
          "}";
          "/* comment */";
          "@media screen {";
          "  .box:hover { color: #fff; }";
          "}";
          "!important";
        ],
      [ H.Attribute; H.Builtin; H.Number; H.String; H.Comment; H.Keyword ] );
    ( "sql",
      H.find ".SQL",
      source
        [
          "SELECT id, name";
          "FROM users";
          "WHERE active = true";
          "  AND count >= 3;";
          "-- comment";
          "INSERT INTO users(name) VALUES ('charm');";
          "UPDATE users SET name = \"new\";";
          "CREATE TABLE items (id INTEGER);";
          "SELECT COUNT(*) FROM items;";
          "ROLLBACK;";
        ],
      [
        H.Keyword;
        H.Type;
        H.Builtin;
        H.Constant;
        H.Number;
        H.Comment;
        H.String;
        H.Operator;
      ] );
    ( "markdown",
      H.find ".md",
      source
        [
          "# Heading";
          "";
          "Paragraph with **strong** text.";
          "## Subheading";
          "";
          "```ocaml";
          "let x = 1";
          "```";
          "- item";
          "[link](https://example.com)";
        ],
      [ H.Attribute; H.String ] );
    ( "diff",
      H.find ".patch",
      source
        [
          "diff --git a/a.ml b/a.ml";
          "index 1234567..89abcde 100644";
          "--- a/a.ml";
          "+++ b/a.ml";
          "@@ -1,2 +1,3 @@";
          " let x = 1";
          "+let y = 2";
          "-let z = 3";
          "\\ No newline at end of file";
          "Binary files a/x and b/x differ";
        ],
      [ H.Attribute; H.Operator ] );
    ( "dockerfile",
      H.find "Dockerfile",
      source
        [
          "FROM alpine";
          "ARG VERSION=3";
          "ENV HOME=/root";
          "RUN echo \"hello\"";
          "COPY . /app";
          "WORKDIR /app";
          "# comment";
          "CMD [\"app\"]";
          "FROM base AS final";
          "RUN echo ${VERSION}";
        ],
      [ H.Keyword; H.Attribute; H.Number; H.String; H.Comment ] );
    ( "makefile",
      H.find "foo.mk",
      source
        [
          "CC = cc";
          "CFLAGS := -O2";
          "all: $(TARGET)";
          "\t$(CC) $(CFLAGS) -o app main.c";
          "TARGET = app";
          "include config.mk";
          "# comment";
          "ifeq ($(DEBUG),1)";
          "endif";
          ".PHONY: all";
        ],
      [ H.Keyword; H.Attribute; H.Constant; H.Operator; H.Comment ] );
    ( "lua",
      H.find ".lua",
      source
        [
          "local value = 3";
          "-- comment";
          "--[[ block comment ]]";
          "function greet(name)";
          "  local raw = [=[text]=]";
          "  print(\"hello \" .. name)";
          "  if value > 0 then";
          "    return true";
          "  end";
          "end";
        ],
      [ H.Keyword; H.Number; H.Comment; H.String; H.Builtin; H.Operator; H.Constant ] );
    ( "zig",
      H.find ".zig",
      source
        [
          "const std = @import(\"std\");";
          "pub fn main() void {";
          "  const value: u32 = 3;";
          "  const name = @\"quoted\";";
          "  // comment";
          "  if (value > 0) {";
          "    std.debug.print(\"hi\\n\", .{});";
          "  }";
          "}";
          "const flag = true;";
        ],
      [ H.Keyword; H.Builtin; H.Type; H.Number; H.Ident; H.Comment; H.String; H.Constant ]
    );
  ]

let resolved_sample (name, found, sample, expected) =
  match found with
  | None -> Alcotest.failf "missing language for %s" name
  | Some spec -> (name, spec, sample, expected)

let token_case (name, found, sample, expected) =
  Alcotest.test_case name `Quick (fun () ->
      let name, spec, sample, expected =
        resolved_sample (name, found, sample, expected)
      in
      let tokens = H.tokenize spec sample in
      let reconstructed = String.concat "" (List.map snd tokens) in
      Alcotest.check Alcotest.string (name ^ " concatenation") sample reconstructed;
      List.iter
        (fun kind ->
          let present = List.exists (fun (actual, _) -> actual = kind) tokens in
          Alcotest.check Alcotest.bool (name ^ " token kind") true present)
        expected)

let alias_cases =
  [
    (".ML", "ml");
    ("golang", "go");
    ("foo.rs", "rs");
    ("PY", "py");
    ("mjs", "mjs");
    ("foo.tsx", "tsx");
    ("json", "json");
    (".YML", "yml");
    ("toml", "toml");
    ("zsh", "zsh");
    (".h", "h");
    ("foo.hpp", "hpp");
    ("java", "java");
    (".kt", "kt");
    ("swift", "swift");
    (".rb", "rb");
    ("php", "php");
    (".htm", "htm");
    ("css", "css");
    ("foo.SQL", "sql");
    (".md", "md");
    ("patch", "patch");
    ("Dockerfile", "dockerfile");
    ("foo.mk", "mk");
    (".lua", "lua");
    ("zig", "zig");
  ]

let alias_tests =
  List.map
    (fun (query, expected_name) ->
      Alcotest.test_case query `Quick (fun () ->
          match H.find query with
          | None -> Alcotest.failf "find %s returned None" query
          | Some spec ->
              let matched =
                List.exists
                  (fun name ->
                    String.lowercase_ascii name = String.lowercase_ascii expected_name)
                  spec.H.names
              in
              Alcotest.check Alcotest.bool query true matched))
    alias_cases

let adversarial_cases =
  [
    ( "nested OCaml comments",
      H.find "ocaml",
      "(* outer (* nested *) still comment *) let x = 1",
      [ H.Comment; H.Keyword; H.Number ] );
    ( "nested comment skips OCaml raw strings",
      H.find "ocaml",
      "(* {| contains *) and remains raw |} *) let x = 1",
      [ H.Comment; H.Keyword; H.Number ] );
    ( "OCaml quoted raw string",
      H.find "ml",
      "{tag|not (* comment *)|tag} let x = {|raw|}",
      [ H.String; H.Keyword ] );
    ( "Rust arbitrary hash raw string",
      H.find "rust",
      "let raw = r###\"(* not a comment *)\"###;",
      [ H.Keyword; H.String ] );
    ( "escaped string",
      H.find "python",
      "\"escaped \\\" quote\" 42",
      [ H.String; H.Number ] );
    ("unterminated block", H.find "c", "/* unterminated", [ H.Comment ]);
    ("make variable", H.find "make", "$(CC) ${CFLAGS}", [ H.Attribute ]);
    ("longest Go operator", H.find "go", "a >>= b", [ H.Operator ]);
    ("diff line anchor", H.find "diff", "text\ndiff --git a/x b/x\n", [ H.Attribute ]);
    ("empty source", H.find "json", "", []);
  ]

let adversarial_tests =
  List.map
    (fun (name, found, sample, expected) ->
      Alcotest.test_case name `Quick (fun () ->
          match found with
          | None -> Alcotest.failf "missing adversarial language %s" name
          | Some spec ->
              let tokens = H.tokenize spec sample in
              Alcotest.check Alcotest.string (name ^ " concatenation") sample
                (String.concat "" (List.map snd tokens));
              List.iter
                (fun kind ->
                  Alcotest.check Alcotest.bool (name ^ " token kind") true
                    (List.exists (fun (actual, _) -> actual = kind) tokens))
                expected))
    adversarial_cases

let language_count =
  Alcotest.test_case "all 26 language specifications" `Quick (fun () ->
      Alcotest.check Alcotest.int "language count" 26 (List.length H.languages))

let render_case =
  Alcotest.test_case "render applies styles and preserves empty-theme bytes" `Quick
    (fun () ->
      match H.find "ml" with
      | None -> Alcotest.fail "OCaml specification missing"
      | Some spec ->
          let source = "let x = 1" in
          let empty_theme _ = Charamel_ansi.Style.default in
          let rendered = H.render ~theme:empty_theme spec source in
          Alcotest.check Alcotest.string "empty theme is byte identity" source rendered)

let strip_sgr text =
  let out = Buffer.create (String.length text) in
  let i = ref 0 in
  while !i < String.length text do
    if !i + 1 < String.length text && text.[!i] = '\027' && text.[!i + 1] = '[' then (
      let j = ref (!i + 2) in
      while !j < String.length text && text.[!j] <> 'm' do
        incr j
      done;
      i := !j + 1)
    else begin
      Buffer.add_char out text.[!i];
      incr i
    end
  done;
  Buffer.contents out

let identity_case =
  Alcotest.test_case "rendering rewrites only tabs and line endings" `Quick (fun () ->
      match H.find "ml" with
      | None -> Alcotest.fail "OCaml specification missing"
      | Some spec ->
          let source = "(* a\nbbbb *)\nlet\tanswer = 42\r\nlet v = \"x\ty\" in\n" in
          let cells = "(* a\nbbbb *)\nlet    answer = 42\nlet v = \"x    y\" in\n" in
          let rendered = H.render ~theme:(H.Theme.charm ~is_dark:true) spec source in
          Alcotest.check Alcotest.string "cells survive" cells (strip_sgr rendered))

let hex lang literal =
  Alcotest.test_case
    ("hex " ^ lang ^ " " ^ literal)
    `Quick
    (fun () ->
      match H.find lang with
      | None -> Alcotest.failf "missing language %s" lang
      | Some spec -> (
          match H.tokenize spec literal with
          | [ (kind, text) ] ->
              Alcotest.check Alcotest.string (lang ^ " literal text") literal text;
              Alcotest.check Alcotest.bool (lang ^ " number kind") true (kind = H.Number)
          | tokens ->
              Alcotest.failf "%s: %S split into %d tokens" lang literal
                (List.length tokens)))

let not_hex lang literal =
  Alcotest.test_case
    ("not hex " ^ lang ^ " " ^ literal)
    `Quick
    (fun () ->
      match H.find lang with
      | None -> Alcotest.failf "missing language %s" lang
      | Some spec -> (
          match H.tokenize spec literal with
          | [ (kind, text) ] when kind = H.Number && text = literal ->
              Alcotest.failf "%s: %S lexed as one hexadecimal literal" lang literal
          | _ -> ()))

let number_tests =
  List.map
    (fun (lang, literal) -> hex lang literal)
    [
      ("java", "0xdeadBeefL");
      ("java", "0xF");
      ("javascript", "0xA0n");
      ("kotlin", "0xFFu");
      ("kotlin", "0xdead");
      ("php", "0xABC");
      ("ruby", "0xdef");
      ("swift", "0x10");
      ("toml", "0x00FF");
      ("zig", "0xdead");
      ("bash", "0x2a");
    ]
  @ [
      not_hex "java" "0x";
      not_hex "php" "0xGG";
      not_hex "swift" "0xz";
      not_hex "kotlin" "0xU";
    ]

let theme_corpus =
  "(* c *)\n\
   let answer = 42\n\
   type t = { name : string option }\n\
   let f x = x ^ \"s\" ^ true\n\
   [@@@warning \"-27\"]\n\
   print_endline None\n"

let theme_styles =
  [
    ("charm-dark", H.Theme.charm ~is_dark:true);
    ("charm-light", H.Theme.charm ~is_dark:false);
    ("dracula", H.Theme.dracula);
    ("github-dark", H.Theme.github ~is_dark:true);
    ("github-light", H.Theme.github ~is_dark:false);
    ("monokai", H.Theme.monokai);
    ("nord", H.Theme.nord);
    ("solarized-dark", H.Theme.solarized ~is_dark:true);
    ("solarized-light", H.Theme.solarized ~is_dark:false);
  ]

let corpus_kinds =
  [
    ("keyword", H.Keyword);
    ("type", H.Type);
    ("builtin", H.Builtin);
    ("constant", H.Constant);
    ("string", H.String);
    ("number", H.Number);
    ("comment", H.Comment);
    ("operator", H.Operator);
    ("punct", H.Punct);
    ("ident", H.Ident);
    ("attribute", H.Attribute);
    ("text", H.Text);
  ]

let check_corpus_kinds kinds =
  List.iter
    (fun (label, kind) ->
      if not (List.mem kind kinds) then
        Alcotest.failf "corpus does not produce a %s token" label)
    corpus_kinds

let read_golden name =
  In_channel.with_open_bin ("data/" ^ name ^ ".golden") In_channel.input_all

let theme_case name theme =
  Alcotest.test_case ("theme bytes " ^ name) `Quick (fun () ->
      match H.find "ml" with
      | None -> Alcotest.fail "OCaml specification missing"
      | Some spec ->
          let kinds = List.map fst (H.tokenize spec theme_corpus) in
          check_corpus_kinds kinds;
          let rendered = H.render ~theme spec theme_corpus in
          Alcotest.check Alcotest.string (name ^ " bytes") (read_golden name) rendered)

let theme_tests = List.map (fun (name, theme) -> theme_case name theme) theme_styles

let () =
  Alcotest.run "highlight"
    [
      ("languages", language_count :: List.map token_case samples);
      ("aliases", alias_tests);
      ("adversarial", adversarial_tests);
      ("themes", [ render_case; identity_case ]);
      ("number literals", number_tests);
      ("theme goldens", theme_tests);
    ]
