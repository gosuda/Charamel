open Charamel_huh

let key name =
  match Charamel_tea.Key.of_string name with
  | Ok value -> value
  | Error (`Msg message) -> Alcotest.failf "invalid test key %s: %s" name message

let form_env =
  Form.Env.v ~fs_root:(Sys.getcwd ())
    ~temp_dir:(Filename.get_temp_dir_name ())
    ~editor:None ~clock:Charamel_os.Time.lwt

let frame form events =
  snd (Charamel_tea.Test.run (Run.app form_env form) ~events ~size:(24, 80))

let single field = Form.v [ Group.v [ field ] ]

let languages ?(height = 3) ?inline ?(filterable = false) ?default key =
  Field.select ~title:(Dyn.const "Language") ~height ~filterable ?inline ?default
    ~options:(Dyn.const (Field.options_of_strings [ "ocaml"; "go"; "rust" ]))
    key

let numbers key =
  Field.select ~title:(Dyn.const "Numbers") ~height:1
    ~options:
      (Dyn.const (Field.options_of_strings [ "one"; "two"; "three"; "four"; "five" ]))
    key

let toppings ?(height = 3) ?(limit = 0) ?(filterable = true) ?(default = []) key =
  Field.multi_select ~title:(Dyn.const "Toppings") ~height ~limit ~filterable ~default
    ~options:(Dyn.const (Field.options_of_strings [ "lettuce"; "tomato"; "onion" ]))
    key

let dump name frame =
  match Sys.getenv_opt "CHARM_HUH_BATTERY_DUMP" with
  | None -> ()
  | Some path ->
      let channel = open_out_gen [ Open_append; Open_creat ] 0o644 path in
      Fun.protect
        ~finally:(fun () -> close_out channel)
        (fun () -> output_string channel (Printf.sprintf "=== %s ===\n%s\n\n" name frame))

let check name expected get () =
  let actual = get () in
  dump name actual;
  Alcotest.(check string) name expected actual

let test_select_initial () =
  check "select-initial"
    "\226\148\131 Language\n\
     \226\148\131 > ocaml \n\
     \226\148\131   go    \n\
     \226\148\131   rust\n\n\
     enter submit \226\128\162 up up \226\128\162 down down \226\128\162 / filter \
     \226\128\162 ctrl+u half page up \226\128\166"
    (fun () -> frame (single (languages (Key.v "k"))) [])
    ()

let test_select_moves () =
  check "select-moves"
    "\226\148\131 Language\n\
     \226\148\131   ocaml \n\
     \226\148\131   go    \n\
     \226\148\131 > rust\n\n\
     enter submit \226\128\162 up up \226\128\162 down down \226\128\162 / filter \
     \226\128\162 ctrl+u half page up \226\128\166"
    (fun () ->
      frame (single (languages (Key.v "k"))) [ `Key (key "down"); `Key (key "down") ])
    ()

let test_select_default () =
  check "select-default"
    "\226\148\131 Language\n\
     \226\148\131   ocaml \n\
     \226\148\131 > go    \n\
     \226\148\131   rust\n\n\
     enter submit \226\128\162 up up \226\128\162 down down \226\128\162 / filter \
     \226\128\162 ctrl+u half page up \226\128\166"
    (fun () -> frame (single (languages ~default:"go" (Key.v "k"))) [])
    ()

let test_select_filter () =
  check "select-filter"
    "\226\148\131 \
     Language                                                                           \n\
     \226\148\131 / \
     oc                                                                               \n\
     \226\148\131 > ocaml\n\n\
     enter submit \226\128\162 esc set filter \226\128\162 up up \226\128\162 down down \
     \226\128\162 g/home go to start \226\128\166"
    (fun () ->
      frame
        (single (languages ~filterable:true (Key.v "k")))
        [ `Key (key "/"); `Text "oc" ])
    ()

let test_select_filter_esc () =
  check "select-filter-esc"
    "\226\148\131 Language\n\
     \226\148\131 > ocaml \n\
     \226\148\131   go    \n\
     \226\148\131   rust\n\n\
     enter submit \226\128\162 up up \226\128\162 down down \226\128\162 / filter \
     \226\128\162 ctrl+u half page up \226\128\166"
    (fun () ->
      frame
        (single (languages ~filterable:true (Key.v "k")))
        [ `Key (key "/"); `Text "zz"; `Key (key "esc") ])
    ()

let test_select_inline () =
  check "select-inline"
    "\226\148\131 Language     \n\
     \226\148\131 <- > ocaml ->\n\n\
     enter submit \226\128\162 up up \226\128\162 down down \226\128\162 / filter \
     \226\128\162 ctrl+u half page up \226\128\166"
    (fun () -> frame (single (languages ~inline:true (Key.v "k"))) [])
    ()

let test_select_scroll () =
  check "select-scroll"
    "\226\148\131 Numbers\n\
     \226\148\131 > four\n\n\
     enter submit \226\128\162 up up \226\128\162 down down \226\128\162 / filter \
     \226\128\162 ctrl+u half page up \226\128\166"
    (fun () ->
      frame
        (single (numbers (Key.v "k")))
        [ `Key (key "down"); `Key (key "down"); `Key (key "down") ])
    ()

let test_multi_initial () =
  check "multi-initial"
    "\226\148\131 Toppings   \n\
     \226\148\131 > \226\128\162 lettuce\n\
     \226\148\131   \226\128\162 tomato \n\
     \226\148\131   \226\128\162 onion\n\n\
     enter submit \226\128\162 x toggle \226\128\162 up up \226\128\162 down down \
     \226\128\162 / filter \226\128\162 ctrl+u half page up \226\128\166"
    (fun () -> frame (single (toppings (Key.v "k"))) [])
    ()

let test_multi_toggles () =
  check "multi-toggles"
    "\226\148\131 Toppings   \n\
     \226\148\131   \226\156\147 lettuce\n\
     \226\148\131 > \226\156\147 tomato \n\
     \226\148\131   \226\128\162 onion\n\n\
     enter submit \226\128\162 x toggle \226\128\162 up up \226\128\162 down down \
     \226\128\162 / filter \226\128\162 ctrl+u half page up \226\128\166"
    (fun () ->
      frame
        (single (toppings (Key.v "k")))
        [ `Key (key "space"); `Key (key "down"); `Key (key "space") ])
    ()

let test_multi_limit_one () =
  check "multi-limit-one"
    "\226\148\131 Toppings   \n\
     \226\148\131   \226\156\147 lettuce\n\
     \226\148\131   \226\128\162 tomato \n\
     \226\148\131 > \226\128\162 onion\n\n\
     enter submit \226\128\162 x toggle \226\128\162 up up \226\128\162 down down \
     \226\128\162 / filter \226\128\162 ctrl+u half page up \226\128\166"
    (fun () ->
      frame
        (single (toppings ~limit:1 (Key.v "k")))
        [
          `Key (key "space");
          `Key (key "down");
          `Key (key "space");
          `Key (key "down");
          `Key (key "space");
        ])
    ()

let test_multi_select_all () =
  check "multi-select-all"
    "\226\148\131 Toppings   \n\
     \226\148\131 > \226\156\147 lettuce\n\
     \226\148\131   \226\156\147 tomato \n\
     \226\148\131   \226\156\147 onion\n\n\
     enter submit \226\128\162 x toggle \226\128\162 up up \226\128\162 down down \
     \226\128\162 / filter \226\128\162 ctrl+u half page up \226\128\166"
    (fun () -> frame (single (toppings (Key.v "k"))) [ `Key (key "ctrl+a") ])
    ()

let test_multi_select_none () =
  check "multi-select-none"
    "\226\148\131 Toppings   \n\
     \226\148\131 > \226\128\162 lettuce\n\
     \226\148\131   \226\128\162 tomato \n\
     \226\148\131   \226\128\162 onion\n\n\
     enter submit \226\128\162 x toggle \226\128\162 up up \226\128\162 down down \
     \226\128\162 / filter \226\128\162 ctrl+u half page up \226\128\166"
    (fun () ->
      frame (single (toppings (Key.v "k"))) [ `Key (key "ctrl+a"); `Key (key "ctrl+a") ])
    ()

let test_multi_filter () =
  check "multi-filter"
    "\226\148\131 Toppings   \n\
     \226\148\131 > \226\128\162 lettuce\n\
     \226\148\131   \226\128\162 tomato \n\
     \226\148\131\n\n\
     enter submit \226\128\162 x toggle \226\128\162 up up \226\128\162 down down \
     \226\128\162 / filter \226\128\162 esc clear filter \226\128\166"
    (fun () ->
      frame
        (single (toppings (Key.v "k")))
        [ `Key (key "/"); `Text "t"; `Key (key "enter") ])
    ()

let test_multi_default () =
  check "multi-default"
    "\226\148\131 Toppings   \n\
     \226\148\131 > \226\156\147 lettuce\n\
     \226\148\131   \226\128\162 tomato \n\
     \226\148\131   \226\156\147 onion\n\n\
     enter submit \226\128\162 x toggle \226\128\162 up up \226\128\162 down down \
     \226\128\162 / filter \226\128\162 ctrl+u half page up \226\128\166"
    (fun () ->
      frame
        (single (toppings ~default:[ "lettuce"; "onion" ] (Key.v "k")))
        [ `Key (key "enter") ])
    ()

let results_of model =
  match Form.state model.Run.form with
  | `Completed results -> results
  | `Normal | `Aborted -> Alcotest.fail "battery form did not complete"

let test_limit_one_matches_select () =
  let one = Key.v "one" in
  let many = Key.v "many" in
  let select_model, select_frame =
    Charamel_tea.Test.run
      (Run.app form_env (single (languages ~height:1 one)))
      ~events:[ `Key (key "down"); `Key (key "enter") ]
      ~size:(24, 80)
  in
  let multi_model, multi_frame =
    Charamel_tea.Test.run
      (Run.app form_env (single (toppings ~height:1 ~limit:1 many)))
      ~events:[ `Key (key "down"); `Key (key "space"); `Key (key "enter") ]
      ~size:(24, 80)
  in
  dump "limit1-select" select_frame;
  dump "limit1-multi" multi_frame;
  Alcotest.(check (option string))
    "select value" (Some "go")
    (Results.get one (results_of select_model));
  Alcotest.(check (list string))
    "limit one value" [ "tomato" ]
    (Option.value ~default:[] (Results.get many (results_of multi_model)))

let cases =
  [
    ("select initial", `Quick, test_select_initial);
    ("select moves", `Quick, test_select_moves);
    ("select default", `Quick, test_select_default);
    ("select filter", `Quick, test_select_filter);
    ("select filter esc", `Quick, test_select_filter_esc);
    ("select inline", `Quick, test_select_inline);
    ("select scroll", `Quick, test_select_scroll);
    ("multi initial", `Quick, test_multi_initial);
    ("multi toggles", `Quick, test_multi_toggles);
    ("multi limit one", `Quick, test_multi_limit_one);
    ("multi select all", `Quick, test_multi_select_all);
    ("multi select none", `Quick, test_multi_select_none);
    ("multi filter", `Quick, test_multi_filter);
    ("multi default", `Quick, test_multi_default);
    ("limit one matches select", `Quick, test_limit_one_matches_select);
  ]

let () = Alcotest.run "merge-battery" [ ("picker goldens", cases) ]
