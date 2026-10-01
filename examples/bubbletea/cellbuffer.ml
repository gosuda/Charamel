module H = Charamel_harmonica
module K = Charamel_tea.Cmd
module S = Charamel_tea.Sub
module V = Charamel_tea.View

let fps = 60
let delta_time = H.fps fps
let asterisk = "*"

type cell_buffer = { mutable cells : string array; mutable stride : int }

let resize buffer ~width ~height =
  buffer.stride <- width;
  buffer.cells <- Array.make (width * height) " "

let make_cells () = { cells = [||]; stride = 0 }
let width_of buffer = buffer.stride

let height_of buffer =
  if buffer.stride = 0 then 0 else Array.length buffer.cells / buffer.stride

let ready buffer = Array.length buffer.cells > 0
let wipe buffer = Array.fill buffer.cells 0 (Array.length buffer.cells) " "

let set buffer ~x ~y =
  let index = (y * width_of buffer) + x in
  if
    x >= 0 && y >= 0
    && x < width_of buffer
    && y < height_of buffer
    && index < Array.length buffer.cells
  then buffer.cells.(index) <- asterisk

let to_string buffer =
  let text = Buffer.create (Array.length buffer.cells + width_of buffer) in
  Array.iteri
    (fun index cell ->
      if index > 0 && index mod width_of buffer = 0 then Buffer.add_char text '\n';
      Buffer.add_string text cell)
    buffer.cells;
  Buffer.contents text

let draw_ellipse buffer ~xc ~yc ~rx ~ry =
  let x = ref 0.0 in
  let y = ref ry in
  let d1 = ref ((ry *. ry) -. (rx *. rx *. ry) +. (0.25 *. rx *. rx)) in
  let dx = ref (2.0 *. ry *. ry *. !x) in
  let dy = ref (2.0 *. rx *. rx *. !y) in
  let point px py = set buffer ~x:(int_of_float px) ~y:(int_of_float py) in
  while !dx < !dy do
    point (!x +. xc) (!y +. yc);
    point (-. !x +. xc) (!y +. yc);
    point (!x +. xc) (-. !y +. yc);
    point (-. !x +. xc) (-. !y +. yc);
    if !d1 < 0.0 then begin
      x := !x +. 1.0;
      dx := !dx +. (2.0 *. ry *. ry);
      d1 := !d1 +. !dx +. (ry *. ry)
    end
    else begin
      x := !x +. 1.0;
      y := !y -. 1.0;
      dx := !dx +. (2.0 *. ry *. ry);
      dy := !dy -. (2.0 *. rx *. rx);
      d1 := !d1 +. !dx -. !dy +. (ry *. ry)
    end
  done;
  let d2 =
    ref
      ((ry *. ry *. ((!x +. 0.5) *. (!x +. 0.5)))
      +. (rx *. rx *. ((!y -. 1.0) *. (!y -. 1.0)))
      -. (rx *. rx *. ry *. ry))
  in
  while !y >= 0.0 do
    point (!x +. xc) (!y +. yc);
    point (-. !x +. xc) (!y +. yc);
    point (!x +. xc) (-. !y +. yc);
    point (-. !x +. xc) (-. !y +. yc);
    if !d2 > 0.0 then begin
      y := !y -. 1.0;
      dy := !dy -. (2.0 *. rx *. rx);
      d2 := !d2 +. (rx *. rx) -. !dy
    end
    else begin
      y := !y -. 1.0;
      x := !x +. 1.0;
      dx := !dx +. (2.0 *. ry *. ry);
      dy := !dy -. (2.0 *. rx *. rx);
      d2 := !d2 +. !dx -. !dy +. (rx *. rx)
    end
  done

let spring = H.Spring.v ~delta_time ~angular_frequency:7.5 ~damping_ratio:0.15

type model = {
  buffer : cell_buffer;
  target_x : float;
  target_y : float;
  x : float;
  y : float;
  x_velocity : float;
  y_velocity : float;
}

type msg =
  | Frame
  | Key of Charamel_tea.Key.t
  | Mouse of Charamel_tea.Mouse.t
  | Resize of { rows : int; cols : int }

let animate = K.after delta_time (fun () -> Frame)

let update msg model =
  match msg with
  | Key _ -> (model, K.quit)
  | Resize { rows; cols } ->
      let fresh = not (ready model.buffer) in
      resize model.buffer ~width:cols ~height:rows;
      let model =
        if fresh then
          {
            model with
            target_x = float_of_int cols /. 2.0;
            target_y = float_of_int rows /. 2.0;
          }
        else model
      in
      (model, K.none)
  | Mouse mouse ->
      let target_x = float_of_int mouse.x and target_y = float_of_int mouse.y in
      if ready model.buffer then ({ model with target_x; target_y }, K.none)
      else (model, K.none)
  | Frame ->
      if not (ready model.buffer) then (model, K.none)
      else begin
        wipe model.buffer;
        let x, x_velocity =
          H.Spring.update spring ~pos:model.x ~vel:model.x_velocity ~target:model.target_x
        in
        let y, y_velocity =
          H.Spring.update spring ~pos:model.y ~vel:model.y_velocity ~target:model.target_y
        in
        draw_ellipse model.buffer ~xc:x ~yc:y ~rx:16.0 ~ry:8.0;
        ({ model with x; y; x_velocity; y_velocity }, animate)
      end

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        ( {
            buffer = make_cells ();
            target_x = 0.0;
            target_y = 0.0;
            x = 0.0;
            y = 0.0;
            x_velocity = 0.0;
            y_velocity = 0.0;
          },
          animate ));
    update;
    view =
      (fun model -> V.v ~alt_screen:true ~mouse:V.Mouse_motion (to_string model.buffer));
    subscriptions =
      (fun _ ->
        S.batch
          [
            S.key (fun key -> Key key);
            S.mouse (fun mouse -> Mouse mouse);
            S.resize (fun ~rows ~cols -> Resize { rows; cols });
          ]);
  }

let main () = Smoke.run_ app

let no_mods =
  {
    Charamel_tea.Key.shift = false;
    alt = false;
    ctrl = false;
    meta = false;
    super = false;
    hyper = false;
    caps_lock = false;
    num_lock = false;
  }

let click x y =
  `Msg (Mouse { Charamel_tea.Mouse.x; y; button = Left; action = Press; mods = no_mods })

let _ = click
let _ = click
let _ = click
let _ = click
let arc column = Fmt.str "\n%s***********" (String.make column ' ')

let smoke () =
  Smoke.expect app ~size:(48, 100)
    [ `Resize (48, 100); `Wait 4.0; Smoke.key "q" ]
    [ arc 45; asterisk ]
  @ Smoke.expect app ~size:(48, 100)
      [ `Resize (48, 100); click 10 30; `Wait 4.0; Smoke.key "q" ]
      [ arc 5 ]
  @ Smoke.expect app ~size:(48, 100)
      [ `Resize (48, 100); click 40 10; `Wait 4.0; Smoke.key "q" ]
      [ arc 35 ]
