type t = {
  mutable pending_finish : Stream_part.t option;
  mutable usage : Usage.t option;
  mutable finished : bool;
}

let create () = { pending_finish = None; usage = None; finished = false }
let usage_events st = match st.usage with Some u -> [ Stream_part.Usage u ] | None -> []

let terminal st msg =
  st.finished <- true;
  st.pending_finish <- None;
  let usage = usage_events st in
  st.usage <- None;
  usage @ [ Stream_part.Finish (`Error msg) ]

let release st =
  match st.pending_finish with
  | None -> []
  | Some f ->
      st.finished <- true;
      st.pending_finish <- None;
      let usage = usage_events st in
      st.usage <- None;
      usage @ [ f ]
