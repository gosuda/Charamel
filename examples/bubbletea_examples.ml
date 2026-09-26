let usage () =
  prerr_endline "usage: bubbletea_examples (NAME | --smoke NAME | --list)";
  exit 2

let lookup name =
  match Bubbletea.Examples.find name with
  | Some example -> example
  | None ->
      Printf.eprintf "bubbletea_examples: unknown example %S\n" name;
      exit 2

let contains ~needle ~shown =
  let n = String.length needle and h = String.length shown in
  let rec go i =
    i + n <= h && (String.equal (String.sub shown i n) needle || go (i + 1))
  in
  go 0

let report name pairs =
  match pairs with
  | [] ->
      Printf.printf "%s: smoke asserted nothing\n" name;
      exit 1
  | pairs -> (
      match List.filter (fun (needle, shown) -> not (contains ~needle ~shown)) pairs with
      | [] -> Printf.printf "%s: ok (%d assertions)\n" name (List.length pairs)
      | missing ->
          List.iter
            (fun (needle, shown) ->
              Printf.printf "%s: missing %S in frame:\n%s\n" name needle shown)
            missing;
          exit 1)

let smoke name =
  let example = lookup name in
  report name
    (match example.smoke () with
    | pairs -> pairs
    | exception exn ->
        Printf.printf "%s: smoke raised %s\n%s\n" name (Printexc.to_string exn)
          (Printexc.get_backtrace ());
        exit 1)

let list () =
  List.iter
    (fun { Bubbletea.Examples.name; _ } -> print_endline name)
    Bubbletea.Examples.all

let () =
  Printexc.record_backtrace true;
  match Array.to_list Sys.argv with
  | [ _; "--list" ] -> list ()
  | [ _; "--smoke"; name ] -> smoke name
  | [ _; name ] ->
      if Unix.isatty Unix.stdin then Lwt_main.run ((lookup name).main ())
      else (
        Printf.printf "%s: not a terminal, running the smoke instead\n" name;
        smoke name)
  | _ -> usage ()
