module Cmd = Charamel_tea.Cmd
module Color = Charamel_ansi.Color
module Key = Charamel_tea.Key
module Sides_color = Charamel_lipgloss.Sides_color
module Style = Charamel_lipgloss.Style
module Table = Charamel_bubbles.Table
module View = Charamel_tea.View

type model = { table : Table.t }
type msg = Key of Key.t | Move of Table.msg

let base_style =
  Style.(
    empty
    |> border Charamel_lipgloss.Border.normal
    |> border_foreground (Sides_color.all (Color.Indexed 240)))

let columns =
  [
    { Table.title = "Rank"; width = 4 };
    { Table.title = "City"; width = 10 };
    { Table.title = "Country"; width = 10 };
    { Table.title = "Population"; width = 10 };
  ]

let rows : Table.row list =
  [
    [ "1"; "Tokyo"; "Japan"; "37,274,000" ];
    [ "2"; "Delhi"; "India"; "32,065,760" ];
    [ "3"; "Shanghai"; "China"; "28,516,904" ];
    [ "4"; "Dhaka"; "Bangladesh"; "22,478,116" ];
    [ "5"; "São Paulo"; "Brazil"; "22,429,800" ];
    [ "6"; "Mexico City"; "Mexico"; "22,085,140" ];
    [ "7"; "Cairo"; "Egypt"; "21,750,020" ];
    [ "8"; "Beijing"; "China"; "21,333,332" ];
    [ "9"; "Mumbai"; "India"; "20,961,472" ];
    [ "10"; "Osaka"; "Japan"; "19,059,856" ];
    [ "11"; "Chongqing"; "China"; "16,874,740" ];
    [ "12"; "Karachi"; "Pakistan"; "16,839,950" ];
    [ "13"; "Istanbul"; "Turkey"; "15,636,243" ];
    [ "14"; "Kinshasa"; "DR Congo"; "15,628,085" ];
    [ "15"; "Lagos"; "Nigeria"; "15,387,639" ];
    [ "16"; "Buenos Aires"; "Argentina"; "15,369,919" ];
    [ "17"; "Kolkata"; "India"; "15,133,888" ];
    [ "18"; "Manila"; "Philippines"; "14,406,059" ];
    [ "19"; "Tianjin"; "China"; "14,011,828" ];
    [ "20"; "Guangzhou"; "China"; "13,964,637" ];
    [ "21"; "Rio De Janeiro"; "Brazil"; "13,634,274" ];
    [ "22"; "Lahore"; "Pakistan"; "13,541,764" ];
    [ "23"; "Bangalore"; "India"; "13,193,035" ];
    [ "24"; "Shenzhen"; "China"; "12,831,330" ];
    [ "25"; "Moscow"; "Russia"; "12,640,818" ];
    [ "26"; "Chennai"; "India"; "11,503,293" ];
    [ "27"; "Bogota"; "Colombia"; "11,344,312" ];
    [ "28"; "Paris"; "France"; "11,142,303" ];
    [ "29"; "Jakarta"; "Indonesia"; "11,074,811" ];
    [ "30"; "Lima"; "Peru"; "11,044,607" ];
    [ "31"; "Bangkok"; "Thailand"; "10,899,698" ];
    [ "32"; "Hyderabad"; "India"; "10,534,418" ];
    [ "33"; "Seoul"; "South Korea"; "9,975,709" ];
    [ "34"; "Nagoya"; "Japan"; "9,571,596" ];
    [ "35"; "London"; "United Kingdom"; "9,540,576" ];
    [ "36"; "Chengdu"; "China"; "9,478,521" ];
    [ "37"; "Nanjing"; "China"; "9,429,381" ];
    [ "38"; "Tehran"; "Iran"; "9,381,546" ];
    [ "39"; "Ho Chi Minh City"; "Vietnam"; "9,077,158" ];
    [ "40"; "Luanda"; "Angola"; "8,952,496" ];
    [ "41"; "Wuhan"; "China"; "8,591,611" ];
    [ "42"; "Xi An Shaanxi"; "China"; "8,537,646" ];
    [ "43"; "Ahmedabad"; "India"; "8,450,228" ];
    [ "44"; "Kuala Lumpur"; "Malaysia"; "8,419,566" ];
    [ "45"; "New York City"; "United States"; "8,177,020" ];
    [ "46"; "Hangzhou"; "China"; "8,044,878" ];
    [ "47"; "Surat"; "India"; "7,784,276" ];
    [ "48"; "Suzhou"; "China"; "7,764,499" ];
    [ "49"; "Hong Kong"; "Hong Kong"; "7,643,256" ];
    [ "50"; "Riyadh"; "Saudi Arabia"; "7,538,200" ];
    [ "51"; "Shenyang"; "China"; "7,527,975" ];
    [ "52"; "Baghdad"; "Iraq"; "7,511,920" ];
    [ "53"; "Dongguan"; "China"; "7,511,851" ];
    [ "54"; "Foshan"; "China"; "7,497,263" ];
    [ "55"; "Dar Es Salaam"; "Tanzania"; "7,404,689" ];
    [ "56"; "Pune"; "India"; "6,987,077" ];
    [ "57"; "Santiago"; "Chile"; "6,856,939" ];
    [ "58"; "Madrid"; "Spain"; "6,713,557" ];
    [ "59"; "Haerbin"; "China"; "6,665,951" ];
    [ "60"; "Toronto"; "Canada"; "6,312,974" ];
    [ "61"; "Belo Horizonte"; "Brazil"; "6,194,292" ];
    [ "62"; "Khartoum"; "Sudan"; "6,160,327" ];
    [ "63"; "Johannesburg"; "South Africa"; "6,065,354" ];
    [ "64"; "Singapore"; "Singapore"; "6,039,577" ];
    [ "65"; "Dalian"; "China"; "5,930,140" ];
    [ "66"; "Qingdao"; "China"; "5,865,232" ];
    [ "67"; "Zhengzhou"; "China"; "5,690,312" ];
    [ "68"; "Ji Nan Shandong"; "China"; "5,663,015" ];
    [ "69"; "Barcelona"; "Spain"; "5,658,472" ];
    [ "70"; "Saint Petersburg"; "Russia"; "5,535,556" ];
    [ "71"; "Abidjan"; "Ivory Coast"; "5,515,790" ];
    [ "72"; "Yangon"; "Myanmar"; "5,514,454" ];
    [ "73"; "Fukuoka"; "Japan"; "5,502,591" ];
    [ "74"; "Alexandria"; "Egypt"; "5,483,605" ];
    [ "75"; "Guadalajara"; "Mexico"; "5,339,583" ];
    [ "76"; "Ankara"; "Turkey"; "5,309,690" ];
    [ "77"; "Chittagong"; "Bangladesh"; "5,252,842" ];
    [ "78"; "Addis Ababa"; "Ethiopia"; "5,227,794" ];
    [ "79"; "Melbourne"; "Australia"; "5,150,766" ];
    [ "80"; "Nairobi"; "Kenya"; "5,118,844" ];
    [ "81"; "Hanoi"; "Vietnam"; "5,067,352" ];
    [ "82"; "Sydney"; "Australia"; "5,056,571" ];
    [ "83"; "Monterrey"; "Mexico"; "5,036,535" ];
    [ "84"; "Changsha"; "China"; "4,809,887" ];
    [ "85"; "Brasilia"; "Brazil"; "4,803,877" ];
    [ "86"; "Cape Town"; "South Africa"; "4,800,954" ];
    [ "87"; "Jiddah"; "Saudi Arabia"; "4,780,740" ];
    [ "88"; "Urumqi"; "China"; "4,710,203" ];
    [ "89"; "Kunming"; "China"; "4,657,381" ];
    [ "90"; "Changchun"; "China"; "4,616,002" ];
    [ "91"; "Hefei"; "China"; "4,496,456" ];
    [ "92"; "Shantou"; "China"; "4,490,411" ];
    [ "93"; "Xinbei"; "Taiwan"; "4,470,672" ];
    [ "94"; "Kabul"; "Afghanistan"; "4,457,882" ];
    [ "95"; "Ningbo"; "China"; "4,405,292" ];
    [ "96"; "Tel Aviv"; "Israel"; "4,343,584" ];
    [ "97"; "Yaounde"; "Cameroon"; "4,336,670" ];
    [ "98"; "Rome"; "Italy"; "4,297,877" ];
    [ "99"; "Shijiazhuang"; "China"; "4,285,135" ];
    [ "100"; "Montreal"; "Canada"; "4,276,526" ];
  ]

let table_styles =
  let base = Table.default_styles in
  {
    base with
    header =
      Style.(
        base.header
        |> border Charamel_lipgloss.Border.normal
        |> border_foreground (Sides_color.all (Color.Indexed 240))
        |> border_bottom true |> bold false);
    selected =
      Style.(
        base.selected
        |> foreground (Color.Indexed 229)
        |> background (Color.Indexed 57) |> bold false);
  }

let new_table () =
  Table.v ~columns ~rows ~focused:true ~height:7 ~width:42 ~styles:table_styles ()

let move model msg =
  let table, cmd = Table.update msg model.table in
  ({ table }, Cmd.map (fun msg -> Move msg) cmd)

let on_key key model =
  match Key.to_string key with
  | "q" | "ctrl+c" -> (model, Cmd.quit)
  | "escape" ->
      let table =
        if Table.focused model.table then Table.blur model.table
        else Table.focus model.table
      in
      ({ table }, Cmd.none)
  | "enter" ->
      let message =
        match Table.selected_row model.table with
        | Some row ->
            Cmd.print
              (Fmt.str "Let's go to %s!" (List.nth_opt row 1 |> Option.value ~default:""))
        | None -> Cmd.none
      in
      (model, message)
  | _ -> (
      match Table.key model.table key with
      | Some msg -> move model msg
      | None -> (model, Cmd.none))

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> ({ table = new_table () }, Cmd.none));
    update =
      (fun msg model ->
        match msg with
        | Key key -> on_key key model
        | Move move_msg -> move model move_msg);
    view =
      (fun model ->
        View.v
          ?cursor:(Table.view_cursor model.table)
          (Style.render base_style (Table.view model.table)
          ^ "\n  " ^ Table.help_view model.table ^ "\n"));
    subscriptions = (fun _ -> Charamel_tea.Sub.key (fun key -> Key key));
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "Rank"; "City"; "Country"; "Population"; "Tokyo"; "37,274,000" ]
  @ Smoke.expect app [ Smoke.key "f" ] [ "Cairo"; "21,750,020" ]
  @ Smoke.expect app [ Smoke.key "escape"; Smoke.key "f" ] [ "Tokyo"; "Delhi" ]
  @ Smoke.expect app [ Smoke.key "G" ] [ "Montreal"; "Shijiazhu…" ]
