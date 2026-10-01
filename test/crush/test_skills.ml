module Config = Crush_core.Config
module Skills = Crush_core.Skills
open Lwt_direct

let setup f = Test_tools_test_support.with_scratch (fun root -> f root)
let write = Test_tools_test_support.write_file

let discovers_and_resolves_uri () =
  setup (fun root ->
      Test_tools_test_support.mkdir_p (root ^ "/alpha");
      write (root ^ "/alpha/SKILL.md")
        "---\nname: alpha\ndescription: Read files safely\n---\nUse the read tool.\n";
      write (root ^ "/alpha/example.txt") "example\n";
      let config = { Config.default with skills_paths = [ root ] } in
      let skills = await (Skills.load ~fs_root:"/" ~config ~home:(root ^ "/home")) in
      let skill =
        match Skills.find skills "alpha" with
        | Some skill -> skill
        | None -> Alcotest.fail "skill was not discovered"
      in
      Alcotest.check Alcotest.string "description" "Read files safely"
        skill.Skills.description;
      Alcotest.check Alcotest.string "body" "Use the read tool.\n" skill.Skills.body;
      Alcotest.check Alcotest.string "index" "- alpha: Read files safely"
        (Skills.index_text skills);
      (match await (Skills.resolve_uri skills ~fs_root:"/" "skill://alpha") with
      | Ok body -> Alcotest.check Alcotest.string "body URI" skill.Skills.body body
      | Error (`Not_found uri) -> Alcotest.failf "URI not found: %s" uri);
      (match
         await (Skills.resolve_uri skills ~fs_root:"/" "skill://alpha/example.txt")
       with
      | Ok body -> Alcotest.check Alcotest.string "file URI" "example\n" body
      | Error (`Not_found uri) -> Alcotest.failf "file URI not found: %s" uri);
      match await (Skills.resolve_uri skills ~fs_root:"/" "skill://alpha/../secret") with
      | Error (`Not_found _) -> ()
      | Ok _ -> Alcotest.fail "skill URI escaped its directory")

let configured_precedes_user_and_defaults_name () =
  setup (fun root ->
      let configured = root ^ "/configured" in
      let home = root ^ "/home" in
      let user = home ^ "/.crush/skills" in
      List.iter
        (fun path -> Test_tools_test_support.mkdir_p path)
        [ configured ^ "/same"; user ^ "/same"; user ^ "/useronly" ];
      write (configured ^ "/same/SKILL.md") "configured body";
      write (user ^ "/same/SKILL.md") "user body";
      write (user ^ "/useronly/SKILL.md") "default name body";
      let config = { Config.default with skills_paths = [ configured ] } in
      let skills = await (Skills.load ~fs_root:"/" ~config ~home) in
      (match Skills.find skills "same" with
      | Some skill ->
          Alcotest.check Alcotest.string "configured wins" "configured body"
            skill.Skills.body
      | None -> Alcotest.fail "configured skill missing");
      match Skills.find skills "useronly" with
      | Some skill ->
          Alcotest.check Alcotest.string "directory default name" "useronly"
            skill.Skills.name
      | None -> Alcotest.fail "user skill missing")

let oversized_files_are_not_indexed () =
  setup (fun root ->
      let path = root ^ "/huge" in
      Test_tools_test_support.mkdir_p path;
      write (path ^ "/SKILL.md") (String.make 65_537 'x');
      let config = { Config.default with skills_paths = [ root ] } in
      let skills = await (Skills.load ~fs_root:"/" ~config ~home:(root ^ "/home")) in
      Alcotest.check Alcotest.int "oversized skill skipped" 0
        (List.length (Skills.all skills)))

let malformed_frontmatter_is_explicit () =
  setup (fun root ->
      let path = root ^ "/bad" in
      Test_tools_test_support.mkdir_p path;
      write (path ^ "/SKILL.md") "---\nunsupported: value\n---\nbody";
      let config = { Config.default with skills_paths = [ root ] } in
      match
        try
          ignore (await (Skills.load ~fs_root:"/" ~config ~home:(root ^ "/home")));
          None
        with Invalid_argument message -> Some message
      with
      | Some message ->
          Alcotest.check Alcotest.bool "invalid header names its field" true
            (String.contains message 'u')
      | None -> Alcotest.fail "malformed skill header was accepted")

let symlink_cannot_escape_skill_directory () =
  setup (fun root ->
      let skill_dir = root ^ "/alpha" in
      let outside = root ^ "/outside" in
      Test_tools_test_support.mkdir_p skill_dir;
      Test_tools_test_support.mkdir_p outside;
      write (skill_dir ^ "/SKILL.md") "body";
      write (outside ^ "/secret.txt") "secret";
      Unix.symlink outside (skill_dir ^ "/link");
      let config = { Config.default with skills_paths = [ root ] } in
      let skills = await (Skills.load ~fs_root:"/" ~config ~home:(root ^ "/home")) in
      match
        await (Skills.resolve_uri skills ~fs_root:"/" "skill://alpha/link/secret.txt")
      with
      | Error (`Not_found _) -> ()
      | Ok _ -> Alcotest.fail "skill URI followed a symlink outside the skill")

let cases =
  [
    Test_tools_test_support.case "discover and resolve skills" `Quick
      discovers_and_resolves_uri;
    Test_tools_test_support.case "configured skills precede user skills" `Quick
      configured_precedes_user_and_defaults_name;
    Test_tools_test_support.case "oversized skills are bounded" `Quick
      oversized_files_are_not_indexed;
    Test_tools_test_support.case "malformed frontmatter is explicit" `Quick
      malformed_frontmatter_is_explicit;
    Test_tools_test_support.case "skill URI stays within subtree" `Quick
      symlink_cannot_escape_skill_directory;
  ]
