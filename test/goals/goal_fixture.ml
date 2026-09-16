let repo_root () =
  let rec up dir =
    if Sys.file_exists (Filename.concat dir "dune-project") then dir
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then
        Alcotest.fail "goal fixture: dune-project not found above the working directory"
      else up parent
  in
  up (Sys.getcwd ())

let real_path () = Option.value (Sys.getenv_opt "PATH") ~default:"/usr/bin:/bin"

let contains ~needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  if needle_length = 0 then true
  else
    let rec scan index =
      if index + needle_length > text_length then false
      else if String.equal (String.sub text index needle_length) needle then true
      else scan (index + 1)
    in
    scan 0

let fresh_scratch env ~root ~name =
  let base = Filename.concat root ".outline/worktree" in
  let path =
    Filename.concat base (Fmt.str "goal-%s-%d-%d" name (Unix.getpid ()) (Random.bits ()))
  in
  let dir_path = Eio.Path.(env#fs / path) in
  Eio.Path.rmtree ~missing_ok:true dir_path;
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir_path;
  path
