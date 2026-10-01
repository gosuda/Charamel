open Charamel_ansi

let d65_x = 0.95047
let d65_y = 1.0
let d65_z = 1.08883
let sixth29 = 6.0 /. 29.0
let lab_epsilon = sixth29 *. sixth29 *. sixth29
let four29 = 4.0 /. 29.0

let linearize v =
  if v <= 0.04045 then v /. 12.92 else Float.pow ((v +. 0.055) /. 1.055) 2.4

let delinearize v =
  if v <= 0.0031308 then 12.92 *. v else (1.055 *. Float.pow v (1.0 /. 2.4)) -. 0.055

let lab_f t =
  if t > lab_epsilon then Float.cbrt t
  else (t /. 3.0 *. 29.0 /. 6.0 *. 29.0 /. 6.0) +. four29

let lab_f_inv t =
  if t > sixth29 then t *. t *. t else 3.0 *. sixth29 *. sixth29 *. (t -. four29)

let components c =
  match Color.to_rgb c with Some (r, g, b) -> (r, g, b) | None -> (0, 0, 0)

let clamp01 v = Float.max 0.0 (Float.min 1.0 v)

let byte v =
  let c = clamp01 v in
  int_of_float (Float.floor ((c *. 65535.0) +. 0.5)) lsr 8

let lab_of (r, g, b) =
  let lr = linearize (float_of_int r /. 255.0) in
  let lg = linearize (float_of_int g /. 255.0) in
  let lb = linearize (float_of_int b /. 255.0) in
  let x =
    (0.41239079926595948 *. lr) +. (0.35758433938387796 *. lg)
    +. (0.18048078840183429 *. lb)
  in
  let y =
    (0.21263900587151036 *. lr) +. (0.71516867876775593 *. lg)
    +. (0.072192315360733715 *. lb)
  in
  let z =
    (0.019330818715591851 *. lr) +. (0.11919477979462599 *. lg)
    +. (0.95053215224966058 *. lb)
  in
  let fy = lab_f (y /. d65_y) in
  ( (1.16 *. fy) -. 0.16,
    5.0 *. (lab_f (x /. d65_x) -. fy),
    2.0 *. (fy -. lab_f (z /. d65_z)) )

let rgb_of (l, a, b) =
  let l2 = (l +. 0.16) /. 1.16 in
  let x = d65_x *. lab_f_inv (l2 +. (a /. 5.0)) in
  let y = d65_y *. lab_f_inv l2 in
  let z = d65_z *. lab_f_inv (l2 -. (b /. 2.0)) in
  let lr =
    (3.2409699419045214 *. x) -. (1.5373831775700935 *. y) -. (0.49861076029300328 *. z)
  in
  let lg =
    (-0.96924363628087983 *. x) +. (1.8759675015077207 *. y) +. (0.041555057407175613 *. z)
  in
  let lb =
    (0.055630079696993609 *. x) -. (0.20397695888897657 *. y) +. (1.0569715142428786 *. z)
  in
  Color.Rgb (byte (delinearize lr), byte (delinearize lg), byte (delinearize lb))

let lerp (l1, a1, b1) (l2, a2, b2) t =
  (l1 +. (t *. (l2 -. l1)), a1 +. (t *. (a2 -. a1)), b1 +. (t *. (b2 -. b1)))

let take n xs =
  let rec loop left acc = function
    | _ when left <= 0 -> Stdlib.List.rev acc
    | [] -> Stdlib.List.rev acc
    | x :: rest -> loop (left - 1) (x :: acc) rest
  in
  loop n [] xs

let blend1d ~steps stops =
  let steps = if steps < 0 then 0 else steps in
  if steps <= Stdlib.List.length stops then take steps stops
  else
    let stops = Stdlib.List.filter (fun c -> c <> Color.Default) stops in
    match stops with
    | [] -> []
    | [ single ] -> Stdlib.List.init steps (fun _ -> single)
    | many ->
        let labs =
          Array.of_list (Stdlib.List.map (fun c -> lab_of (components c)) many)
        in
        let segments = Array.length labs - 1 in
        let base = steps / segments in
        let extra = steps mod segments in
        let out = Array.make steps Color.Default in
        let index = ref 0 in
        for segment = 0 to segments - 1 do
          let size = if segment < extra then base + 1 else base in
          let divisor = float_of_int (size - 1) in
          for step = 0 to size - 1 do
            let t = if size > 1 then float_of_int step /. divisor else 0.0 in
            out.(!index) <- rgb_of (lerp labs.(segment) labs.(segment + 1) t);
            incr index
          done
        done;
        Array.to_list out

let blend2d ~width ~height ~angle stops =
  let width = if width < 1 then 1 else width in
  let height = if height < 1 then 1 else height in
  let angle =
    let a = Float.rem angle 360.0 in
    if a < 0.0 then a +. 360.0 else a
  in
  let stops = Stdlib.List.filter (fun c -> c <> Color.Default) stops in
  match stops with
  | [] -> []
  | [ single ] -> Stdlib.List.init (width * height) (fun _ -> single)
  | many ->
      let gradient = Array.of_list (blend1d ~steps:(max width height) many) in
      let last = Array.length gradient - 1 in
      let center_x = float_of_int (width - 1) /. 2.0 in
      let center_y = float_of_int (height - 1) /. 2.0 in
      let radians = angle *. Float.pi /. 180.0 in
      let cos_angle = Float.cos radians in
      let sin_angle = Float.sin radians in
      let diagonal = sqrt (float_of_int ((width * width) + (height * height))) in
      let span = float_of_int last in
      Stdlib.List.init (width * height) (fun position ->
          let x = float_of_int (position mod width) -. center_x in
          let y = float_of_int (position / width) -. center_y in
          let rotated = (x *. cos_angle) -. (y *. sin_angle) in
          let t = clamp01 ((rotated +. (diagonal /. 2.0)) /. diagonal) in
          gradient.(min last (int_of_float (t *. span))))
