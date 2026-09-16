type cluster = {
  first : Uchar.t option;
  pair : bool;
  fe0f : bool;
  fe0e : bool;
  count : int;
}

let empty_cluster = { first = None; pair = false; fe0f = false; fe0e = false; count = 0 }
let variation_selector_16 = Uchar.of_int 0xFE0F
let variation_selector_15 = Uchar.of_int 0xFE0E

let scan cluster u =
  {
    first = (match cluster.first with Some first -> Some first | None -> Some u);
    pair =
      (cluster.pair
      ||
      match cluster.count with
      | 1 -> (
          Uucp.Func.is_regional_indicator u
          &&
          match cluster.first with
          | Some first -> Uucp.Func.is_regional_indicator first
          | None -> false)
      | _ -> false);
    fe0f = cluster.fe0f || Uchar.equal u variation_selector_16;
    fe0e = cluster.fe0e || Uchar.equal u variation_selector_15;
    count = cluster.count + 1;
  }

let width_of_cluster cluster =
  match cluster.first with
  | None -> 0
  | Some first ->
      let cp = Uchar.to_int first in
      if cp = 0xFF9E || cp = 0xFF9F then 0
      else if cluster.fe0f then 2
      else if cluster.fe0e then 1
      else if Uucp.Emoji.is_emoji_presentation first then 2
        (* GB12 and GB13 pair regional indicator runs; [pair] holds only when
         the cluster's second scalar is also an indicator, so a lone
         indicator followed by a join control falls through to the base. *)
      else if cluster.pair then 2
      else max 0 (Uucp.Break.tty_width_hint first)

let fold_states f s =
  let seg = Uuseg.create `Grapheme_cluster in
  let emit state = if state.count > 0 then f state in
  let rec pump state v =
    match Uuseg.add seg v with
    | `Uchar u -> pump (scan state u) `Await
    | `Boundary ->
        emit state;
        pump empty_cluster `Await
    | `Await -> state
    | `End ->
        emit state;
        state
  in
  let rec drain state v =
    match Uuseg.add seg v with
    | `Uchar u -> drain (scan state u) `Await
    | `Boundary ->
        emit state;
        drain empty_cluster `Await
    | `Await | `End -> emit state
  in
  let state =
    let len = String.length s in
    let rec feed state i =
      if i >= len then state
      else
        let decoded = String.get_utf_8_uchar s i in
        feed
          (pump state (`Uchar (Uchar.utf_decode_uchar decoded)))
          (i + Uchar.utf_decode_length decoded)
    in
    feed empty_cluster 0
  in
  drain state `End

let string_width s =
  let total = ref 0 in
  fold_states (fun c -> total := !total + width_of_cluster c) s;
  !total

let grapheme_width s =
  let first = ref None in
  fold_states
    (fun c ->
      match !first with None -> first := Some (width_of_cluster c) | Some _ -> ())
    s;
  match !first with Some width -> width | None -> 0

(* Uuseg_string folds the same grapheme segmenter over the string and
   replaces malformed input by [Uchar.rep], matching the pump above. *)
let graphemes s =
  List.rev
    (Uuseg_string.fold_utf_8 `Grapheme_cluster (fun clusters c -> c :: clusters) [] s)
