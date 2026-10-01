module Color = Charamel_ansi.Color
module Style = Charamel_lipgloss.Style

let eye_width = 15
let eye_height = 12
let eye_spacing = 40
let blink_frames = 20
let tick_interval = 0.05

type model = {
  width : int;
  height : int;
  left_x : int;
  right_x : int;
  eye_y : int;
  blinking : bool;
  blink_state : int;
  since_blink : float;
  open_time : float;
}

type msg = Tick | Key of Charamel_tea.Key.t | Resize of int * int

let random_open_time () =
  let open_time = float_of_int (Random.int 3000 + 1000) /. 1000. in
  if Random.int 10 = 0 then 0.3 else open_time

let tick = Charamel_tea.Cmd.after tick_interval (fun () -> Tick)

let layout width height =
  let start_x = (width - eye_spacing) / 2 in
  (start_x, start_x + eye_spacing, height / 2)

let draw_ellipse canvas x0 y0 ry =
  let rx = eye_width in
  let height = Array.length canvas in
  if height > 0 then
    let width = Array.length canvas.(0) in
    for y = -ry to ry do
      let span =
        int_of_float
          (float_of_int rx *. sqrt (1.0 -. ((float_of_int y /. float_of_int ry) ** 2.0)))
      in
      for x = -span to span do
        let cx = x0 + x in
        let cy = y0 + y in
        if cx >= 0 && cx < width && cy >= 0 && cy < height then canvas.(cy).(cx) <- "●"
      done
    done

let current_height model =
  if not model.blinking then eye_height
  else
    let progress =
      if model.blink_state < blink_frames / 2 then
        let x = float_of_int model.blink_state /. float_of_int (blink_frames / 2) in
        1.0 -. (x *. x)
      else
        let x =
          float_of_int (model.blink_state - (blink_frames / 2))
          /. float_of_int (blink_frames / 2)
        in
        x *. (2.0 -. x)
    in
    max 1 (int_of_float (float_of_int eye_height *. progress))

let blink_tick model =
  if model.blinking then
    let blink_state = model.blink_state + 1 in
    if blink_state >= blink_frames then
      {
        model with
        blinking = false;
        blink_state = 0;
        since_blink = 0.;
        open_time = random_open_time ();
      }
    else { model with blink_state }
  else if model.since_blink >= model.open_time then
    { model with blinking = true; blink_state = 1 }
  else { model with since_blink = model.since_blink +. tick_interval }

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        let left_x, right_x, eye_y = layout 80 24 in
        ( {
            width = 80;
            height = 24;
            left_x;
            right_x;
            eye_y;
            blinking = false;
            blink_state = 0;
            since_blink = 0.;
            open_time = random_open_time ();
          },
          tick ));
    update =
      (fun msg model ->
        match msg with
        | Key key -> (
            match Charamel_tea.Key.to_string key with
            | "ctrl+c" | "escape" -> (model, Charamel_tea.Cmd.quit)
            | _ -> (model, Charamel_tea.Cmd.none))
        | Resize (rows, cols) ->
            let left_x, right_x, eye_y = layout cols rows in
            ( { model with width = cols; height = rows; left_x; right_x; eye_y },
              Charamel_tea.Cmd.none )
        | Tick -> (blink_tick model, tick));
    view =
      (fun model ->
        let canvas = Array.make_matrix model.height model.width " " in
        let ry = current_height model in
        draw_ellipse canvas model.left_x model.eye_y ry;
        draw_ellipse canvas model.right_x model.eye_y ry;
        let text =
          String.concat ""
            (List.map
               (fun row -> row ^ "\n")
               (List.map
                  (fun cells -> String.concat "" cells)
                  (Array.to_list (Array.map Array.to_list canvas))))
        in
        Charamel_tea.View.v ~alt_screen:true
          (Style.render Style.(empty |> foreground (Color.of_hex_or "#F0F0F0")) text));
    subscriptions =
      (fun _ ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.resize (fun ~rows ~cols -> Resize (rows, cols));
          ]);
  }

let main () = Smoke.run_ app
let dots n = String.concat "" (List.init n (fun _ -> "●"))

let smoke () =
  Smoke.expect app [ Smoke.key "escape" ] [ dots 31 ^ String.make 9 ' ' ^ dots 31 ]
  @ Smoke.expect app
      [ `Resize (24, 60); Smoke.key "escape" ]
      [ dots 26 ^ String.make 9 ' ' ^ dots 25 ^ "\n" ]
  @ Smoke.expect app
      [ `Wait 0.6; Smoke.key "escape" ]
      [ dots 31 ^ String.make 9 ' ' ^ dots 31 ]
  @ Smoke.expect app [ `Wait 5.0; Smoke.key "escape" ] [ dots 15 ]
