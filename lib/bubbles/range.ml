let clamp lo hi v =
  let lo, hi = if lo <= hi then (lo, hi) else (hi, lo) in
  max lo (min hi v)
