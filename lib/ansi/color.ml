(* The 16-color hex values and the 256-to-16 mapping table below are
   transcribed from .references/x/ansi/color.go (charmbracelet/x/ansi, MIT). *)
let ansi_hex =
  [|
    0x000000;
    0x800000;
    0x008000;
    0x808000;
    0x000080;
    0x800080;
    0x008080;
    0xc0c0c0;
    0x808080;
    0xff0000;
    0x00ff00;
    0xffff00;
    0x0000ff;
    0xff00ff;
    0x00ffff;
    0xffffff;
  |]

let ansi256_to16 =
  [|
    0;
    1;
    2;
    3;
    4;
    5;
    6;
    7;
    8;
    9;
    10;
    11;
    12;
    13;
    14;
    15;
    0;
    4;
    4;
    4;
    12;
    12;
    2;
    6;
    4;
    4;
    12;
    12;
    2;
    2;
    6;
    4;
    12;
    12;
    2;
    2;
    2;
    6;
    12;
    12;
    10;
    10;
    10;
    10;
    14;
    12;
    10;
    10;
    10;
    10;
    10;
    14;
    1;
    5;
    4;
    4;
    12;
    12;
    3;
    8;
    4;
    4;
    12;
    12;
    2;
    2;
    6;
    4;
    12;
    12;
    2;
    2;
    2;
    6;
    12;
    12;
    10;
    10;
    10;
    10;
    14;
    12;
    10;
    10;
    10;
    10;
    10;
    14;
    1;
    1;
    5;
    4;
    12;
    12;
    1;
    1;
    5;
    4;
    12;
    12;
    3;
    3;
    8;
    4;
    12;
    12;
    2;
    2;
    2;
    6;
    12;
    12;
    10;
    10;
    10;
    10;
    14;
    12;
    10;
    10;
    10;
    10;
    10;
    14;
    1;
    1;
    1;
    5;
    12;
    12;
    1;
    1;
    1;
    5;
    12;
    12;
    1;
    1;
    1;
    5;
    12;
    12;
    3;
    3;
    3;
    7;
    12;
    12;
    10;
    10;
    10;
    10;
    14;
    12;
    10;
    10;
    10;
    10;
    10;
    14;
    9;
    9;
    9;
    9;
    13;
    12;
    9;
    9;
    9;
    9;
    13;
    12;
    9;
    9;
    9;
    9;
    13;
    12;
    9;
    9;
    9;
    9;
    13;
    12;
    11;
    11;
    11;
    11;
    7;
    12;
    10;
    10;
    10;
    10;
    10;
    14;
    9;
    9;
    9;
    9;
    9;
    13;
    9;
    9;
    9;
    9;
    9;
    13;
    9;
    9;
    9;
    9;
    9;
    13;
    9;
    9;
    9;
    9;
    9;
    13;
    9;
    9;
    9;
    9;
    9;
    13;
    11;
    11;
    11;
    11;
    11;
    15;
    0;
    0;
    0;
    0;
    0;
    0;
    8;
    8;
    8;
    8;
    8;
    8;
    7;
    7;
    7;
    7;
    7;
    7;
    15;
    15;
    15;
    15;
    15;
    15;
  |]

(* The conversion reproduces Convert256 and Convert16 of
   .references/x/ansi/color.go, including the HSLuv distance that breaks ties
   between the 6x6x6 cube and the grey ramp. The HSLuv math follows the
   go-colorful sources upstream pins (hsluv.go and colors.go); operation order
   is kept to preserve the rounding of the boundary vectors. *)
let clamp01 v = Float.max 0.0 (Float.min v 1.0)

let linearize v =
  if v <= 0.04045 then v /. 12.92 else Float.pow ((v +. 0.055) /. 1.055) 2.4

let cube_root x = Float.pow x (1.0 /. 3.0)

let linear_rgb_to_xyz r g b =
  ( (0.41239079926595948 *. r) +. (0.35758433938387796 *. g) +. (0.18048078840183429 *. b),
    (0.21263900587151036 *. r) +. (0.71516867876775593 *. g) +. (0.072192315360733715 *. b),
    (0.019330818715591851 *. r) +. (0.11919477979462599 *. g) +. (0.95053215224966058 *. b)
  )

let xyz_to_uv x y z =
  let d = x +. (15.0 *. y) +. (3.0 *. z) in
  if d = 0.0 then (0.0, 0.0) else (4.0 *. x /. d, 9.0 *. y /. d)

let hsluv_white_ref = (0.95045592705167, 1.0, 1.089057750759878)

let xyz_to_luv x y z =
  let wx, wy, wz = hsluv_white_ref in
  let fy = y /. wy in
  let l =
    if fy <= 6.0 /. 29.0 *. (6.0 /. 29.0) *. (6.0 /. 29.0) then
      fy *. (29.0 /. 3.0 *. (29.0 /. 3.0) *. (29.0 /. 3.0)) /. 100.0
    else (1.16 *. cube_root fy) -. 0.16
  in
  let ub, vb = xyz_to_uv x y z in
  let un, vn = xyz_to_uv wx wy wz in
  (l, 13.0 *. l *. (ub -. un), 13.0 *. l *. (vb -. vn))

let luv_to_luvlch l u v =
  let c = sqrt ((u *. u) +. (v *. v)) in
  let h =
    if Float.abs (v -. u) > 1e-4 && Float.abs u > 1e-4 then
      Float.rem ((57.29577951308232087721 *. atan2 v u) +. 360.0) 360.0
    else 0.0
  in
  (l, c, h)

let hsluv_bounds l =
  let sub1 = Float.pow (l +. 16.0) 3.0 /. 1560896.0 in
  let sub2 = if sub1 > 0.0088564516790356308 then sub1 else l /. 903.2962962962963 in
  let line m1 m2 m3 k =
    let kf = float_of_int k in
    let top1 = ((284517.0 *. m1) -. (94839.0 *. m3)) *. sub2 in
    let top2 =
      (((838422.0 *. m3) +. (769860.0 *. m2) +. (731718.0 *. m1)) *. l *. sub2)
      -. (769860.0 *. kf *. l)
    in
    let bottom = (((632260.0 *. m3) -. (126452.0 *. m2)) *. sub2) +. (126452.0 *. kf) in
    (top1 /. bottom, top2 /. bottom)
  in
  let rows =
    [
      (3.2409699419045214, -1.5373831775700935, -0.49861076029300328);
      (-0.96924363628087983, 1.8759675015077207, 0.041555057407175613);
      (0.055630079696993609, -0.20397695888897657, 1.0569715142428786);
    ]
  in
  List.concat_map (fun (m1, m2, m3) -> [ line m1 m2 m3 0; line m1 m2 m3 1 ]) rows

let max_chroma_for_lh l h =
  let h_rad = h /. 360.0 *. Float.pi *. 2.0 in
  List.fold_left
    (fun acc (x, y) ->
      let length = y /. (sin h_rad -. (x *. cos h_rad)) in
      if length > 0.0 && length < acc then length else acc)
    max_float (hsluv_bounds l)

let hsluv rgb =
  let r, g, b = rgb in
  let x, y, z = linear_rgb_to_xyz (linearize r) (linearize g) (linearize b) in
  let l, u, v = xyz_to_luv x y z in
  let l, c, h = luv_to_luvlch l u v in
  let l = l *. 100.0 in
  let c = c *. 100.0 in
  let s =
    if l > 99.9999999 || l < 0.00000001 then 0.0 else c /. max_chroma_for_lh l h *. 100.0
  in
  (h, clamp01 (s /. 100.0), clamp01 (l /. 100.0))

let hsluv_distance a b =
  let h1, s1, l1 = hsluv a in
  let h2, s2, l2 = hsluv b in
  let dh = (h1 -. h2) /. 100.0 in
  let ds = s1 -. s2 in
  let dl = l1 -. l2 in
  sqrt ((dh *. dh) +. (ds *. ds) +. (dl *. dl))

let q2c = [| 0x00; 0x5f; 0x87; 0xaf; 0xd7; 0xff |]

let to_6_cube v =
  if v < 48.0 then 0 else if v < 115.0 then 1 else int_of_float ((v -. 35.0) /. 40.0)

let convert256_of_bytes r g b =
  let nr = float_of_int r /. 255.0 in
  let ng = float_of_int g /. 255.0 in
  let nb = float_of_int b /. 255.0 in
  let r = nr *. 255.0 and g = ng *. 255.0 and b = nb *. 255.0 in
  let qr = to_6_cube r in
  let qg = to_6_cube g in
  let qb = to_6_cube b in
  let cr = q2c.(qr) and cg = q2c.(qg) and cb = q2c.(qb) in
  let ci = (36 * qr) + (6 * qg) + qb in
  if
    Int.equal cr (int_of_float r)
    && Int.equal cg (int_of_float g)
    && Int.equal cb (int_of_float b)
  then 16 + ci
  else
    let grey_avg = int_of_float (r +. g +. b) / 3 in
    let grey_idx = if grey_avg > 238 then 23 else (grey_avg - 3) / 10 in
    let grey = 8 + (10 * grey_idx) in
    let cube =
      (float_of_int cr /. 255.0, float_of_int cg /. 255.0, float_of_int cb /. 255.0)
    in
    let gg = float_of_int grey /. 255.0 in
    let color_dist = hsluv_distance (nr, ng, nb) cube in
    let gray_dist = hsluv_distance (nr, ng, nb) (gg, gg, gg) in
    if color_dist <= gray_dist then 16 + ci else 232 + grey_idx

let in_byte_range v = if v < 0 then 0 else if v > 255 then 255 else v
let in_basic_range n = if n < 0 then 0 else if n > 15 then 15 else n
let in_256_range n = if n < 0 then 0 else if n > 255 then 255 else n

type t = Default | Basic of int | Indexed of int | Rgb of int * int * int

let basic n = if n >= 0 && n <= 15 then Some (Basic n) else None
let indexed n = if n >= 0 && n <= 255 then Some (Indexed n) else None

let rgb r g b =
  if (r >= 0 && r <= 255) && (g >= 0 && g <= 255) && b >= 0 && b <= 255 then
    Some (Rgb (r, g, b))
  else None

let hex_digit = function
  | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' as c -> Some (10 + Char.code c - Char.code 'a')
  | 'A' .. 'F' as c -> Some (10 + Char.code c - Char.code 'A')
  | _ -> None

let of_hex s =
  let n = String.length s in
  if n = 4 && s.[0] = '#' then
    begin match (hex_digit s.[1], hex_digit s.[2], hex_digit s.[3]) with
    | Some r, Some g, Some b ->
        let dup v = (v * 16) + v in
        Some (Rgb (dup r, dup g, dup b))
    | _ -> None
    end
  else if n = 7 && s.[0] = '#' then
    begin match
      ( hex_digit s.[1],
        hex_digit s.[2],
        hex_digit s.[3],
        hex_digit s.[4],
        hex_digit s.[5],
        hex_digit s.[6] )
    with
    | Some r1, Some r2, Some g1, Some g2, Some b1, Some b2 ->
        Some (Rgb ((r1 * 16) + r2, (g1 * 16) + g2, (b1 * 16) + b2))
    | _ -> None
    end
  else None

let of_hex_or ?(default = Default) s =
  match of_hex s with Some color -> color | None -> default

let decimal_magnitude s =
  let length = String.length s in
  let start = if length > 0 && (s.[0] = '+' || s.[0] = '-') then 1 else 0 in
  if start >= length then None
  else
    let value = ref 0 and valid = ref true in
    let index = ref start in
    while !index < length && !valid do
      let digit = Char.code s.[!index] - Char.code '0' in
      if digit < 0 || digit > 9 then valid := false
      else if !value > (max_int - digit) / 10 then valid := false
      else value := (!value * 10) + digit;
      incr index
    done;
    if !valid then Some !value else None

let of_string s =
  if String.length s > 0 && s.[0] = '#' then of_hex s
  else
    match decimal_magnitude s with
    | None -> None
    | Some n when n < 16 -> Some (Basic n)
    | Some n when n < 256 -> Some (Indexed n)
    | Some n -> Some (Rgb ((n lsr 16) land 0xff, (n lsr 8) land 0xff, n land 0xff))

let to_ansi256 = function
  | Default -> Default
  | Indexed n -> Indexed (in_256_range n)
  | Basic n ->
      let hex = ansi_hex.(in_basic_range n) in
      Indexed
        (convert256_of_bytes
           ((hex lsr 16) land 0xff)
           ((hex lsr 8) land 0xff)
           (hex land 0xff))
  | Rgb (r, g, b) ->
      Indexed (convert256_of_bytes (in_byte_range r) (in_byte_range g) (in_byte_range b))

let to_ansi16 = function
  | Default -> Default
  | Basic n -> Basic (in_basic_range n)
  | Indexed n -> Basic ansi256_to16.(in_256_range n)
  | Rgb (r, g, b) ->
      Basic
        ansi256_to16.(convert256_of_bytes (in_byte_range r) (in_byte_range g)
                        (in_byte_range b))

let rgb_of_hex hex = ((hex lsr 16) land 0xff, (hex lsr 8) land 0xff, hex land 0xff)

let to_rgb = function
  | Default -> None
  | Basic n -> Some (rgb_of_hex ansi_hex.(in_basic_range n))
  | Indexed n ->
      let k = in_256_range n in
      if k < 16 then Some (rgb_of_hex ansi_hex.(k))
      else if k < 232 then begin
        let m = k - 16 in
        Some (q2c.(m / 36), q2c.(m / 6 mod 6), q2c.(m mod 6))
      end
      else begin
        let g = 8 + (10 * (k - 232)) in
        Some (g, g, g)
      end
  | Rgb (r, g, b) -> Some (in_byte_range r, in_byte_range g, in_byte_range b)

let is_dark t =
  match to_rgb t with
  | None -> true
  | Some (red, green, blue) -> (299 * red) + (587 * green) + (114 * blue) <= 128_000

let equal = ( = )

let pp fmt = function
  | Default -> Fmt.string fmt "default"
  | Basic n -> Fmt.pf fmt "basic %d" n
  | Indexed n -> Fmt.pf fmt "indexed %d" n
  | Rgb (r, g, b) -> Fmt.pf fmt "rgb %d %d %d" r g b
