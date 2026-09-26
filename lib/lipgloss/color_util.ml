open Charamel_ansi

type rgba = { r : int; g : int; b : int; alpha : float }

let clamp_byte v = if v < 0 then 0 else if v > 255 then 255 else v
let clamp01 v = Float.max 0.0 (Float.min 1.0 v)

let rgba ?(alpha = 1.0) ~r ~g ~b () =
  { r = clamp_byte r; g = clamp_byte g; b = clamp_byte b; alpha = clamp01 alpha }

let components c =
  match Color.to_rgb c with Some (r, g, b) -> (r, g, b) | None -> (0, 0, 0)

let alpha ?(scale = 1.0) c =
  let r, g, b = components c in
  { r; g; b; alpha = clamp01 scale }

let opaque c = c
let to_color v = Color.Rgb (v.r, v.g, v.b)

let hsv r g b =
  let rf = float_of_int r /. 255.0 in
  let gf = float_of_int g /. 255.0 in
  let bf = float_of_int b /. 255.0 in
  let mx = Float.max rf (Float.max gf bf) in
  let mn = Float.min rf (Float.min gf bf) in
  let d = mx -. mn in
  let sector =
    if d = 0.0 then 0.0
    else if mx = rf then (gf -. bf) /. d
    else if mx = gf then 2.0 +. ((bf -. rf) /. d)
    else 4.0 +. ((rf -. gf) /. d)
  in
  let h = Float.rem (sector *. 60.0) 360.0 in
  let h = if h < 0.0 then h +. 360.0 else h in
  let s = if mx = 0.0 then 0.0 else d /. mx in
  (h, s, mx)

let byte_of_float v = int_of_float (Float.round (clamp01 v *. 255.0))

let of_hsv h s v =
  let sector = h /. 60.0 in
  let index = int_of_float (Float.rem (Float.floor sector) 6.0) in
  let f = sector -. Float.floor sector in
  let p = v *. (1.0 -. s) in
  let q = v *. (1.0 -. (f *. s)) in
  let t = v *. (1.0 -. ((1.0 -. f) *. s)) in
  match index with
  | 0 -> (v, t, p)
  | 1 -> (q, v, p)
  | 2 -> (p, v, t)
  | 3 -> (p, q, v)
  | 4 -> (t, p, v)
  | _ -> (v, p, q)

let complementary c =
  let r, g, b = components c in
  let h, s, v = hsv r g b in
  let h = h +. 180.0 in
  let h = if h >= 360.0 then h -. 360.0 else h in
  let rf, gf, bf = of_hsv h s v in
  Color.Rgb (byte_of_float rf, byte_of_float gf, byte_of_float bf)

let darken c percent =
  let mult = 1.0 -. clamp01 percent in
  let r, g, b = components c in
  Color.Rgb
    ( int_of_float (float_of_int r *. mult),
      int_of_float (float_of_int g *. mult),
      int_of_float (float_of_int b *. mult) )

let lighten c percent =
  let add = 255.0 *. clamp01 percent in
  let r, g, b = components c in
  let lift v = min 255 (int_of_float (float_of_int v +. add)) in
  Color.Rgb (lift r, lift g, lift b)

let is_dark c =
  let r, g, b = components c in
  let mx = max r (max g b) and mn = min r (min g b) in
  float_of_int (mx + mn) < 255.0

type triple = Color.t * Color.t * Color.t

let complete profile (ansi, ansi256, truecolor) =
  match profile with
  | Charamel_colorprofile.No_tty | Charamel_colorprofile.Ascii -> Color.Default
  | Charamel_colorprofile.Ansi -> ansi
  | Charamel_colorprofile.Ansi256 -> ansi256
  | Charamel_colorprofile.True_color -> truecolor

let complete_adaptive profile ~dark ~light ~night =
  complete profile (if dark then night else light)
