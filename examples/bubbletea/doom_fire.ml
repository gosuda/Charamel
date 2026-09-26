module K = Charamel_tea.Cmd
module St = Charamel_lipgloss.Style

let palette =
  [|
    0;
    233;
    234;
    52;
    53;
    88;
    89;
    94;
    95;
    96;
    130;
    131;
    132;
    133;
    172;
    214;
    215;
    220;
    220;
    221;
    3;
    226;
    227;
    230;
    231;
    7;
  |]

let palette_size = Array.length palette
let frame_time = 0.05
let half_block = "▀"
let help_text = "Press q or ctrl+c to quit. Elapsed:"

type model = { buf : int array; width : int; height : int; elapsed : float }
type msg = Tick | Key of Charamel_tea.Key.t | Size of { rows : int; cols : int }

let empty = { buf = [||]; width = 0; height = 0; elapsed = 0.0 }
let ready model = model.width > 0 && Array.length model.buf > 0

let resize ~rows ~cols =
  let height = rows * 2 in
  let buf = Array.make (cols * height) 0 in
  let bottom = (height - 1) * cols in
  for column = 0 to cols - 1 do
    buf.(bottom + column) <- palette_size - 1
  done;
  { empty with buf; width = cols; height }

let above model index = index - model.width

let spread model index =
  if index < model.width then ()
  else
    let value = model.buf.(index) in
    if value = 0 then model.buf.(above model index) <- 0
    else begin
      let rnd = Random.int 3 in
      let target = index - rnd + 1 - model.width in
      if target >= 0 && target < Array.length model.buf then
        model.buf.(target) <- max (value - (rnd land 1)) 0
    end

let spread_all model =
  for column = 0 to model.width - 1 do
    for row = 0 to model.height - 1 do
      spread model ((row * model.width) + column)
    done
  done

let cell model row column =
  let index = (row * model.width) + column in
  if index < 0 || index >= Array.length model.buf then 0 else model.buf.(index)

let cells model row =
  Stdlib.List.init model.width @@ fun column ->
  let fg = Charamel_ansi.Color.Indexed palette.(cell model row column) in
  let bg = Charamel_ansi.Color.Indexed palette.(cell model (row + 1) column) in
  St.render St.(empty |> foreground fg |> background bg) half_block

let help_line elapsed =
  let style = St.(empty |> foreground (Charamel_ansi.Color.Basic 7)) in
  St.render style (Fmt.str "%s %.0fs" help_text elapsed)

let view model =
  if not (ready model) then Charamel_tea.View.v "Initializing..."
  else
    let fire =
      Stdlib.List.init
        ((model.height / 2) - 2)
        (fun row -> Stdlib.String.concat "" (cells model (row * 2)))
    in
    let lines = fire @ [ help_line model.elapsed ] in
    Charamel_tea.View.v ~alt_screen:true (Stdlib.String.concat "\n" lines)

let tick = K.after frame_time (fun () -> Tick)

let update msg model =
  match msg with
  | Key key -> (
      match Charamel_tea.Key.to_string key with
      | "q" | "ctrl+c" -> (model, K.quit)
      | _ -> (model, K.none))
  | Size { rows; cols } -> (resize ~rows ~cols, K.none)
  | Tick ->
      if ready model then begin
        spread_all model;
        ({ model with elapsed = model.elapsed +. frame_time }, tick)
      end
      else (model, tick)

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> (empty, tick));
    update;
    view;
    subscriptions =
      (fun _ ->
        K.batch [] |> ignore;
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.resize (fun ~rows ~cols -> Size { rows; cols });
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app ~size:(12, 40) [ Smoke.key "q" ] [ help_text ]
  @ Smoke.expect app ~size:(12, 40) [ `Wait 0.2; Smoke.key "q" ] [ half_block ]
  @ Smoke.expect app ~size:(24, 80) [ `Wait 2.0; Smoke.key "q" ] [ half_block ]
