module Config = Crush_core.Config
module Skills = Crush_core.Skills

let remove_tree path =
  let rec remove path =
    match try Some (Unix.lstat path) with Unix.Unix_error _ -> None with
    | None -> ()
    | Some stat -> (
        match stat.Unix.st_kind with
        | Unix.S_DIR ->
            Array.iter
              (fun entry -> remove (Filename.concat path entry))
              (Sys.readdir path);
            Unix.rmdir path
        | _ -> Sys.remove path)
  in
  remove path

let setup f =
  Eio_main.run (fun env ->
      let root =
        Filename.concat
          (Filename.get_temp_dir_name ())
          ("crush-skills-" ^ string_of_int (Unix.getpid ()))
      in
      remove_tree root;
      Unix.mkdir root 0o700;
      Fun.protect (fun () -> f env root) ~finally:(fun () -> remove_tree root))

let write fs path content =
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(fs / path) content

let discovers_and_resolves_uri () =
  setup (fun env root ->
      let fs = Eio.Stdenv.fs env in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(fs / root / "alpha");
      write fs (root ^ "/alpha/SKILL.md")
        "---\nname: alpha\ndescription: Read files safely\n---\nUse the read tool.\n";
      write fs (root ^ "/alpha/example.txt") "example\n";
      let config = { Config.default with skills_paths = [ root ] } in
      let skills = Skills.load ~fs ~config ~home:(root ^ "/home") in
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
      (match Skills.resolve_uri skills ~fs "skill://alpha" with
      | Ok body -> Alcotest.check Alcotest.string "body URI" skill.Skills.body body
      | Error (`Not_found uri) -> Alcotest.failf "URI not found: %s" uri);
      (match Skills.resolve_uri skills ~fs "skill://alpha/example.txt" with
      | Ok body -> Alcotest.check Alcotest.string "file URI" "example\n" body
      | Error (`Not_found uri) -> Alcotest.failf "file URI not found: %s" uri);
      match Skills.resolve_uri skills ~fs "skill://alpha/../secret" with
      | Error (`Not_found _) -> ()
      | Ok _ -> Alcotest.fail "skill URI escaped its directory")

let configured_precedes_user_and_defaults_name () =
  setup (fun env root ->
      let fs = Eio.Stdenv.fs env in
      let configured = root ^ "/configured" in
      let home = root ^ "/home" in
      let user = home ^ "/.crush/skills" in
      List.iter
        (fun path -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(fs / path))
        [ configured ^ "/same"; user ^ "/same"; user ^ "/useronly" ];
      write fs (configured ^ "/same/SKILL.md") "configured body";
      write fs (user ^ "/same/SKILL.md") "user body";
      write fs (user ^ "/useronly/SKILL.md") "default name body";
      let config = { Config.default with skills_paths = [ configured ] } in
      let skills = Skills.load ~fs ~config ~home in
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
  setup (fun env root ->
      let fs = Eio.Stdenv.fs env in
      let path = root ^ "/huge" in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(fs / path);
      write fs (path ^ "/SKILL.md") (String.make 65_537 'x');
      let config = { Config.default with skills_paths = [ root ] } in
      let skills = Skills.load ~fs ~config ~home:(root ^ "/home") in
      Alcotest.check Alcotest.int "oversized skill skipped" 0
        (List.length (Skills.all skills)))

let malformed_frontmatter_is_explicit () =
  setup (fun env root ->
      let fs = Eio.Stdenv.fs env in
      let path = root ^ "/bad" in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(fs / path);
      write fs (path ^ "/SKILL.md") "---\nunsupported: value\n---\nbody";
      let config = { Config.default with skills_paths = [ root ] } in
      match
        try
          ignore (Skills.load ~fs ~config ~home:(root ^ "/home"));
          None
        with Invalid_argument message -> Some message
      with
      | Some message ->
          Alcotest.check Alcotest.bool "invalid header names its field" true
            (String.contains message 'u')
      | None -> Alcotest.fail "malformed skill header was accepted")

let symlink_cannot_escape_skill_directory () =
  setup (fun env root ->
      let fs = Eio.Stdenv.fs env in
      let skill_dir = root ^ "/alpha" in
      let outside = root ^ "/outside" in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(fs / skill_dir);
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(fs / outside);
      write fs (skill_dir ^ "/SKILL.md") "body";
      write fs (outside ^ "/secret.txt") "secret";
      Unix.symlink outside (skill_dir ^ "/link");
      let config = { Config.default with skills_paths = [ root ] } in
      let skills = Skills.load ~fs ~config ~home:(root ^ "/home") in
      match Skills.resolve_uri skills ~fs "skill://alpha/link/secret.txt" with
      | Error (`Not_found _) -> ()
      | Ok _ -> Alcotest.fail "skill URI followed a symlink outside the skill")

let cases =
  [
    Alcotest.test_case "discover and resolve skills" `Quick discovers_and_resolves_uri;
    Alcotest.test_case "configured skills precede user skills" `Quick
      configured_precedes_user_and_defaults_name;
    Alcotest.test_case "oversized skills are bounded" `Quick
      oversized_files_are_not_indexed;
    Alcotest.test_case "malformed frontmatter is explicit" `Quick
      malformed_frontmatter_is_explicit;
    Alcotest.test_case "skill URI stays within subtree" `Quick
      symlink_cannot_escape_skill_directory;
  ]
