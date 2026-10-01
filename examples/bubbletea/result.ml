let choices = [ "Taro"; "Coffee"; "Lychee" ]

type model = { cursor : int; choice : string }
type msg = Key of Charamel_tea.Key.t

let content model =
  let lines =
    List.mapi
      (fun index name ->
        Fmt.str "%s%s\n" (if model.cursor = index then "(•) " else "( ) ") name)
      choices
  in
  "What kind of Bubble Tea would you like to order?\n\n" ^ String.concat "" lines
  ^ "\n(press q to quit)\n"

let update msg model =
  match msg with
  | Key key -> (
      match Charamel_tea.Key.to_string key with
      | "ctrl+c" | "q" | "esc" -> (model, Charamel_tea.Cmd.quit)
      | "enter" ->
          ({ model with choice = List.nth choices model.cursor }, Charamel_tea.Cmd.quit)
      | "down" | "j" ->
          ({ model with cursor = (model.cursor + 1) mod 3 }, Charamel_tea.Cmd.none)
      | "up" | "k" ->
          ({ model with cursor = (model.cursor + 2) mod 3 }, Charamel_tea.Cmd.none)
      | _ -> (model, Charamel_tea.Cmd.none))

let view model = Charamel_tea.View.v (content model)
let subscriptions _ = Charamel_tea.Sub.key (fun key -> Key key)
let init () = ({ cursor = 0; choice = "" }, Charamel_tea.Cmd.none)
let app : (model, msg) Charamel_tea.app = { init; update; view; subscriptions }

let main () =
  Lwt.map
    (function
      | Some { choice; _ } when not (String.equal choice "") ->
          print_string (Fmt.str "\n---\nYou chose %s!\n" choice)
      | _ -> ())
    (Smoke.run app)

let smoke () =
  Smoke.expect app [] [ "(•) Taro"; "press q to quit" ]
  @ Smoke.expect app [ Smoke.key "down" ] [ "(•) Coffee"; "( ) Taro" ]
  @ Smoke.expect app [ Smoke.key "up" ] [ "(•) Lychee"; "( ) Coffee" ]
  @ Smoke.expect app [ Smoke.key "down"; Smoke.key "enter" ] [ "(•) Coffee" ]
