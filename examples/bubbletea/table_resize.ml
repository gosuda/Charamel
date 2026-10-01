module Color = Charamel_ansi.Color
module Cmd = Charamel_tea.Cmd
module Key = Charamel_tea.Key
module Lipgloss_table = Charamel_lipgloss.Table
module Sides = Charamel_lipgloss.Sides
module Style = Charamel_lipgloss.Style
module Sub = Charamel_tea.Sub
module View = Charamel_tea.View

type model = { table : Lipgloss_table.t }
type msg = Key of Key.t | Win of { rows : int; cols : int }

let base_style = Style.(empty |> padding (Sides.xy ~x:1 ~y:0))
let header_style = Style.(base_style |> foreground (Color.Indexed 252) |> bold true)

let selected_style =
  Style.(
    base_style
    |> foreground (Color.of_hex_or "#01BE85")
    |> background (Color.of_hex_or "#00432F"))

let type_colors =
  List.map
    (fun (name, hex) -> (name, Color.of_hex_or hex))
    [
      ("Bug", "#D7FF87");
      ("Electric", "#FDFF90");
      ("Fire", "#FF7698");
      ("Flying", "#FF87D7");
      ("Grass", "#75FBAB");
      ("Ground", "#FF875F");
      ("Normal", "#929292");
      ("Poison", "#7D5AFC");
      ("Water", "#00E2C7");
    ]

let dim_type_colors =
  List.map
    (fun (name, hex) -> (name, Color.of_hex_or hex))
    [
      ("Bug", "#97AD64");
      ("Electric", "#FCFF5F");
      ("Fire", "#BA5F75");
      ("Flying", "#C97AB2");
      ("Grass", "#59B980");
      ("Ground", "#C77252");
      ("Normal", "#727272");
      ("Poison", "#634BD0");
      ("Water", "#439F8E");
    ]

let headers = [ "#"; "NAME"; "TYPE 1"; "TYPE 2"; "JAPANESE"; "OFFICIAL ROM." ]

let rows : string array array =
  [|
    [| "1"; "Bulbasaur"; "Grass"; "Poison"; "フシギダネ"; "Bulbasaur" |];
    [| "2"; "Ivysaur"; "Grass"; "Poison"; "フシギソウ"; "Ivysaur" |];
    [| "3"; "Venusaur"; "Grass"; "Poison"; "フシギバナ"; "Venusaur" |];
    [| "4"; "Charmander"; "Fire"; ""; "ヒトカゲ"; "Hitokage" |];
    [| "5"; "Charmeleon"; "Fire"; ""; "リザード"; "Lizardo" |];
    [| "6"; "Charizard"; "Fire"; "Flying"; "リザードン"; "Lizardon" |];
    [| "7"; "Squirtle"; "Water"; ""; "ゼニガメ"; "Zenigame" |];
    [| "8"; "Wartortle"; "Water"; ""; "カメール"; "Kameil" |];
    [| "9"; "Blastoise"; "Water"; ""; "カメックス"; "Kamex" |];
    [| "10"; "Caterpie"; "Bug"; ""; "キャタピー"; "Caterpie" |];
    [| "11"; "Metapod"; "Bug"; ""; "トランセル"; "Trancell" |];
    [| "12"; "Butterfree"; "Bug"; "Flying"; "バタフリー"; "Butterfree" |];
    [| "13"; "Weedle"; "Bug"; "Poison"; "ビードル"; "Beedle" |];
    [| "14"; "Kakuna"; "Bug"; "Poison"; "コクーン"; "Cocoon" |];
    [| "15"; "Beedrill"; "Bug"; "Poison"; "スピアー"; "Spear" |];
    [| "16"; "Pidgey"; "Normal"; "Flying"; "ポッポ"; "Poppo" |];
    [| "17"; "Pidgeotto"; "Normal"; "Flying"; "ピジョン"; "Pigeon" |];
    [| "18"; "Pidgeot"; "Normal"; "Flying"; "ピジョット"; "Pigeot" |];
    [| "19"; "Rattata"; "Normal"; ""; "コラッタ"; "Koratta" |];
    [| "20"; "Raticate"; "Normal"; ""; "ラッタ"; "Ratta" |];
    [| "21"; "Spearow"; "Normal"; "Flying"; "オニスズメ"; "Onisuzume" |];
    [| "22"; "Fearow"; "Normal"; "Flying"; "オニドリル"; "Onidrill" |];
    [| "23"; "Ekans"; "Poison"; ""; "アーボ"; "Arbo" |];
    [| "24"; "Arbok"; "Poison"; ""; "アーボック"; "Arbok" |];
    [| "25"; "Pikachu"; "Electric"; ""; "ピカチュウ"; "Pikachu" |];
    [| "26"; "Raichu"; "Electric"; ""; "ライチュウ"; "Raichu" |];
    [| "27"; "Sandshrew"; "Ground"; ""; "サンド"; "Sand" |];
    [| "28"; "Sandslash"; "Ground"; ""; "サンドパン"; "Sandpan" |];
  |]

let row_cells index =
  if index < 0 || index >= Array.length rows then None else Some rows.(index)

let style_func ~row ~col =
  if row < 0 then header_style
  else
    match row_cells row with
    | None -> base_style
    | Some cells ->
        let dim_row = (row + 1) mod 2 = 0 in
        if cells.(1) = "Pikachu" then selected_style
        else if col = 2 || col = 3 then
          match
            List.assoc_opt cells.(col) (if dim_row then dim_type_colors else type_colors)
          with
          | Some color -> Style.(base_style |> foreground color)
          | None -> base_style
        else if dim_row then Style.(base_style |> foreground (Color.Indexed 245))
        else Style.(base_style |> foreground (Color.Indexed 252))

let new_table ?width ?height () =
  Lipgloss_table.v ~headers
    ~rows:(Array.to_list (Array.map (fun cells -> Array.to_list cells) rows))
    ?width ?height ~style:style_func ~border:Charamel_lipgloss.Border.thick ()

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> ({ table = new_table () }, Cmd.none));
    update =
      (fun msg model ->
        match msg with
        | Key key -> (
            match Key.to_string key with
            | "q" | "ctrl+c" -> (model, Cmd.quit)
            | _ -> (model, Cmd.none))
        | Win { rows; cols } ->
            ({ table = new_table ~width:cols ~height:rows () }, Cmd.none));
    view =
      (fun model ->
        View.v ~alt_screen:true ("\n" ^ Lipgloss_table.render model.table ^ "\n"));
    subscriptions =
      (fun _ ->
        Sub.batch
          [
            Sub.key (fun key -> Key key);
            Sub.resize (fun ~rows ~cols -> Win { rows; cols });
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect ~size:(24, 80) app []
    [
      "#";
      "NAME";
      "TYPE 1";
      "TYPE 2";
      "JAPANESE";
      "OFFICIAL ROM.";
      "Bulbasaur";
      "フシギダネ";
      "Charmander";
      "Butterfree";
    ]
  @ Smoke.expect ~size:(14, 80) app [] [ "Bulbasaur"; "Blastoise" ]
  @ Smoke.expect ~size:(24, 40) app [] [ "Bulba"; "…" ]
  @ Smoke.expect ~size:(24, 80) app [ `Resize (20, 44) ] [ "NAME"; "…" ]
