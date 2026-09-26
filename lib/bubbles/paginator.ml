module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub

type kind = Arabic | Dots
type keymap = { prev_page : Key_binding.t; next_page : Key_binding.t }

let default_keymap =
  {
    prev_page = Key_binding.v [ "pgup"; "left"; "h" ];
    next_page = Key_binding.v [ "pgdown"; "right"; "l" ];
  }

type msg = Prev_page | Next_page

type t = {
  kind : kind;
  page : int;
  per_page : int;
  total_pages : int;
  active_dot : string;
  inactive_dot : string;
  arabic_format : int -> int -> string;
  keymap : keymap;
}

let v ?(kind = Arabic) ?(per_page = 1) ?(total_pages = 1) ?(active_dot = "•")
    ?(inactive_dot = "○") ?(arabic_format = fun page total -> Fmt.str "%d/%d" page total)
    ?(keymap = default_keymap) () =
  {
    kind;
    page = 0;
    per_page = max 1 per_page;
    total_pages = max 1 total_pages;
    active_dot;
    inactive_dot;
    arabic_format;
    keymap;
  }

let page t = t.page
let set_page page t = { t with page = Range.clamp 0 (max 0 (t.total_pages - 1)) page }
let per_page t = t.per_page
let set_per_page per_page t = { t with per_page = max 1 per_page }
let total_pages t = t.total_pages

let set_total_pages ~items t =
  if items < 1 then t
  else
    let total_pages = max 1 ((items + t.per_page - 1) / t.per_page) in
    { t with total_pages; page = Range.clamp 0 (total_pages - 1) t.page }

let items_on_page ~total t =
  if total < 1 then 0
  else
    let start, stop =
      let start = t.page * t.per_page in
      (start, min total (start + t.per_page))
    in
    max 0 (stop - start)

let slice_bounds ~length t =
  let length = max 0 length in
  let start = min length (max 0 (t.page * t.per_page)) in
  let stop = min length (start + t.per_page) in
  (start, stop)

let prev_page t = set_page (t.page - 1) t
let next_page t = set_page (t.page + 1) t
let on_first_page t = t.page <= 0
let on_last_page t = t.page >= t.total_pages - 1

let update message t =
  let t = match message with Prev_page -> prev_page t | Next_page -> next_page t in
  (t, Cmd.none)

let view t =
  match t.kind with
  | Arabic -> t.arabic_format (t.page + 1) t.total_pages
  | Dots ->
      let out = Buffer.create (String.length t.active_dot * t.total_pages) in
      for i = 0 to t.total_pages - 1 do
        Buffer.add_string out (if i = t.page then t.active_dot else t.inactive_dot)
      done;
      Buffer.contents out

let key t key =
  if Key_binding.matches key t.keymap.prev_page then Some Prev_page
  else if Key_binding.matches key t.keymap.next_page then Some Next_page
  else None

let subscriptions _ = Sub.none
let set_kind kind t = { t with kind }
let set_active_dot active_dot t = { t with active_dot }
let set_inactive_dot inactive_dot t = { t with inactive_dot }
