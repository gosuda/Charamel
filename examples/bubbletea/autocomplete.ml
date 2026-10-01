open Lwt.Syntax
module Color = Charamel_ansi.Color
module Cmd = Charamel_tea.Cmd
module Help = Charamel_bubbles.Help
module Key_binding = Charamel_bubbles.Key_binding
module Layout = Charamel_lipgloss.Layout
module Position = Charamel_lipgloss.Position
module Sides = Charamel_lipgloss.Sides
module Style = Charamel_lipgloss.Style
module Sub = Charamel_tea.Sub
module Textinput = Charamel_bubbles.Textinput
module View = Charamel_tea.View

let repos_url = "https://api.github.com/orgs/charmbracelet/repos"

let headers =
  [ ("Accept", "application/vnd.github+json"); ("X-GitHub-Api-Version", "2022-11-28") ]

let header_view = "Enter a Charm™ repo:\n"

type keymap = {
  complete : Key_binding.t;
  next : Key_binding.t;
  prev : Key_binding.t;
  quit : Key_binding.t;
}

type model = { input : Textinput.t; help : Help.t; keymap : keymap }

type msg =
  | Repos of (string list, string) result
  | Key of Charamel_tea.Key.t
  | Input of Textinput.msg

let prompt_style =
  Style.(empty |> foreground (Color.Indexed 63) |> margin (Sides.v ~left:2 ()))

let input_styles =
  let base = Textinput.default_styles ~is_dark:true in
  {
    base with
    focused = { base.focused with prompt = prompt_style };
    cursor = { base.cursor with color = Color.Indexed 63 };
  }

let initial_keymap =
  {
    complete = Key_binding.v ~help:("tab", "complete") ~enabled:false [ "tab" ];
    next = Key_binding.v ~help:("ctrl+n", "next") ~enabled:false [ "ctrl+n" ];
    prev = Key_binding.v ~help:("ctrl+p", "prev") ~enabled:false [ "ctrl+p" ];
    quit = Key_binding.v ~help:("esc", "quit") [ "enter"; "ctrl+c"; "escape" ];
  }

let initial_input =
  Textinput.v ~prompt:"charmbracelet/" ~char_limit:50 ~width:20 ~show_suggestions:true
    ~styles:input_styles ~virtual_cursor:false ()

let help_keymap keymap =
  let bindings = [ keymap.complete; keymap.next; keymap.prev; keymap.quit ] in
  { Help.short_help = bindings; full_help = [ bindings ] }

let footer_view model = "\n" ^ Help.view model.help (help_keymap model.keymap)

let refresh_keymap input keymap =
  let enabled = Stdlib.List.length (Textinput.matched_suggestions input) > 1 in
  let bind = Key_binding.set_enabled enabled in
  {
    keymap with
    complete = bind keymap.complete;
    next = bind keymap.next;
    prev = bind keymap.prev;
  }

let input_cmd cmd = Cmd.map (fun message -> Input message) cmd

let apply_input message model =
  let input, cmd = Textinput.update message model.input in
  ({ model with input; keymap = refresh_keymap input model.keymap }, input_cmd cmd)

let on_key key model =
  if Key_binding.matches key model.keymap.quit then (model, Cmd.quit)
  else
    match Textinput.key model.input key with
    | Some message -> apply_input message model
    | None -> (model, Cmd.none)

let on_repos names model =
  let input = Textinput.set_suggestions names model.input in
  ({ model with input; keymap = refresh_keymap input model.keymap }, Cmd.none)

let update message model =
  match message with
  | Repos (Ok names) -> on_repos names model
  | Repos (Error _) -> (model, Cmd.none)
  | Key key -> on_key key model
  | Input message -> apply_input message model

let view model =
  if Textinput.available_suggestions model.input = [] then
    View.v "One sec, we're fetching completions..."
  else
    let text =
      Layout.join_vertical ~pos:Position.left
        [ header_view; Textinput.view model.input; footer_view model ]
    in
    let cursor =
      Option.map
        (fun (cursor : Charamel_tea.Cursor.t) ->
          { cursor with row = cursor.row + Layout.height header_view })
        (Textinput.cursor model.input)
    in
    View.v ?cursor text

let app ~(fetch : unit -> (string list, string) result Lwt.t) :
    (model, msg) Charamel_tea.app =
  let request = Cmd.map (fun names -> Repos names) (Cmd.await (fetch ())) in
  {
    init =
      (fun () ->
        let input, blink = Textinput.focus initial_input in
        ( { input; help = Help.v (); keymap = initial_keymap },
          Cmd.batch [ request; input_cmd blink ] ));
    update;
    view;
    subscriptions =
      (fun model ->
        Sub.batch
          [
            Sub.key (fun key -> Key key);
            Sub.map (fun message -> Input message) (Textinput.subscriptions model.input);
          ]);
  }

let name_of item =
  match item with
  | Jsont.Object (members, _) -> (
      match Jsont.Json.find_mem "name" members with
      | Some (_, Jsont.String (name, _)) -> Some name
      | _ -> None)
  | _ -> None

let decode body =
  match Jsont_bytesrw.decode_string Jsont.json body with
  | Error message -> Error message
  | Ok (Jsont.Array (items, _)) -> Ok (Stdlib.List.filter_map name_of items)
  | Ok _ -> Error "expected a JSON array of repositories"

let read_repos chunks =
  let* drained = Charamel_net.read_body chunks in
  match drained with
  | Error error -> Lwt.return_error (Charamel_net.error_message error)
  | Ok body -> Lwt.return (decode body)

let repos_fetch () =
  let* result =
    Charamel_net.call ~headers ~meth:`GET ~body:None (Uri.of_string repos_url)
  in
  match result with
  | Error error -> Lwt.return_error (Charamel_net.error_message error)
  | Ok (_response, chunks) -> read_repos chunks

let main () = Smoke.run_ (app ~fetch:repos_fetch)
let delivered names () = Lwt.return_ok names
let broken reason () = Lwt.return_error reason

let repo_names =
  [ "bubbles"; "bubbletea"; "glamour"; "harbor"; "lipgloss"; "pop"; "wish" ]

let smoke () =
  Smoke.expect (app ~fetch:(delivered [])) [] [ "One sec, we're fetching completions..." ]
  @ Smoke.expect
      (app ~fetch:(delivered repo_names))
      [ `Wait 0.1 ]
      [ "Enter a Charm™ repo:"; "charmbracelet/" ]
  @ Smoke.expect
      (app ~fetch:(delivered repo_names))
      [ `Msg (Repos (Ok repo_names)); `Text "bub" ]
      [ "charmbracelet/bub"; "tab complete" ]
  @ Smoke.expect
      (app ~fetch:(delivered repo_names))
      [ `Msg (Repos (Ok repo_names)); `Text "bub"; Smoke.key "tab"; `Text "tea" ]
      [ "charmbracelet/bubblestea" ]
  @ Smoke.expect
      (app ~fetch:(broken "getrepos: dial tcp: lookup failed"))
      [ `Wait 0.1 ]
      [ "One sec, we're fetching completions..." ]
