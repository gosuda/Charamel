type t = { top : int; right : int; bottom : int; left : int }

let nonnegative x = max 0 x

let all n =
  let n = nonnegative n in
  { top = n; right = n; bottom = n; left = n }

let xy ~x ~y =
  {
    top = nonnegative y;
    right = nonnegative x;
    bottom = nonnegative y;
    left = nonnegative x;
  }

let v ?(top = 0) ?(right = 0) ?(bottom = 0) ?(left = 0) () =
  {
    top = nonnegative top;
    right = nonnegative right;
    bottom = nonnegative bottom;
    left = nonnegative left;
  }
