type binding = Charm_bubbles.Key_binding.t

type input = {
  accept_suggestion : binding;
  prev : binding;
  next : binding;
  submit : binding;
}

type text = {
  prev : binding;
  next : binding;
  submit : binding;
  new_line : binding;
  editor : binding;
}

type select = {
  prev : binding;
  next : binding;
  submit : binding;
  up : binding;
  down : binding;
  left : binding;
  right : binding;
  filter : binding;
  set_filter : binding;
  clear_filter : binding;
  half_page_up : binding;
  half_page_down : binding;
  goto_top : binding;
  goto_bottom : binding;
}

type multi_select = {
  prev : binding;
  next : binding;
  submit : binding;
  toggle : binding;
  up : binding;
  down : binding;
  filter : binding;
  set_filter : binding;
  clear_filter : binding;
  half_page_up : binding;
  half_page_down : binding;
  goto_top : binding;
  goto_bottom : binding;
  select_all : binding;
  select_none : binding;
}

type confirm = {
  prev : binding;
  next : binding;
  submit : binding;
  toggle : binding;
  accept : binding;
  reject : binding;
}

type note = { prev : binding; next : binding; submit : binding }

type file = {
  goto_top : binding;
  goto_bottom : binding;
  page_up : binding;
  page_down : binding;
  back : binding;
  select : binding;
  up : binding;
  down : binding;
  open_ : binding;
  close : binding;
  prev : binding;
  next : binding;
  submit : binding;
}

type t = {
  quit : binding;
  input : input;
  text : text;
  select : select;
  multi_select : multi_select;
  confirm : confirm;
  note : note;
  file : file;
}

let b ?(enabled = true) ?help keys = Charm_bubbles.Key_binding.v ~enabled ?help keys
let disabled ?help keys = b ~enabled:false ?help keys

let default =
  let quit = b ~help:("ctrl+c", "quit") [ "ctrl+c" ] in
  let input =
    {
      accept_suggestion = b ~help:("ctrl+e", "complete") [ "ctrl+e" ];
      prev = b ~help:("shift+tab", "back") [ "shift+tab" ];
      next = b ~help:("enter", "next") [ "enter"; "tab" ];
      submit = b ~help:("enter", "submit") [ "enter" ];
    }
  in
  let text =
    {
      prev = b ~help:("shift+tab", "back") [ "shift+tab" ];
      next = b ~help:("enter", "next") [ "tab"; "enter" ];
      submit = b ~help:("enter", "submit") [ "enter" ];
      new_line = b ~help:("alt+enter / ctrl+j", "new line") [ "alt+enter"; "ctrl+j" ];
      editor = b ~help:("ctrl+e", "open editor") [ "ctrl+e" ];
    }
  in
  let select =
    {
      prev = b ~help:("shift+tab", "back") [ "shift+tab" ];
      next = b ~help:("enter", "select") [ "enter"; "tab" ];
      submit = b ~help:("enter", "submit") [ "enter" ];
      up = b ~help:("up", "up") [ "up"; "k"; "ctrl+k"; "ctrl+p" ];
      down = b ~help:("down", "down") [ "down"; "j"; "ctrl+j"; "ctrl+n" ];
      left = disabled ~help:("left", "left") [ "h"; "left" ];
      right = disabled ~help:("right", "right") [ "l"; "right" ];
      filter = b ~help:("/", "filter") [ "/" ];
      set_filter = disabled ~help:("esc", "set filter") [ "esc" ];
      clear_filter = disabled ~help:("esc", "clear filter") [ "esc" ];
      half_page_up = b ~help:("ctrl+u", "half page up") [ "ctrl+u" ];
      half_page_down = b ~help:("ctrl+d", "half page down") [ "ctrl+d" ];
      goto_top = b ~help:("g/home", "go to start") [ "home"; "g" ];
      goto_bottom = b ~help:("G/end", "go to end") [ "end"; "shift+g" ];
    }
  in
  let multi_select =
    {
      prev = select.prev;
      next = b ~help:("enter", "confirm") [ "enter"; "tab" ];
      submit = select.submit;
      toggle = b ~help:("x", "toggle") [ "space"; "x" ];
      up = select.up;
      down = select.down;
      filter = select.filter;
      set_filter = disabled ~help:("esc", "set filter") [ "enter"; "esc" ];
      clear_filter = select.clear_filter;
      half_page_up = select.half_page_up;
      half_page_down = select.half_page_down;
      goto_top = select.goto_top;
      goto_bottom = select.goto_bottom;
      select_all = b ~help:("ctrl+a", "select all") [ "ctrl+a" ];
      select_none = disabled ~help:("ctrl+a", "select none") [ "ctrl+a" ];
    }
  in
  let confirm =
    {
      prev = b ~help:("shift+tab", "back") [ "shift+tab" ];
      next = b ~help:("enter", "next") [ "enter"; "tab" ];
      submit = b ~help:("enter", "submit") [ "enter" ];
      toggle = b ~help:("<-/->", "toggle") [ "h"; "l"; "right"; "left" ];
      accept = b ~help:("y", "Yes") [ "y"; "shift+y" ];
      reject = b ~help:("n", "No") [ "n"; "shift+n" ];
    }
  in
  let note =
    {
      prev = b ~help:("shift+tab", "back") [ "shift+tab" ];
      next = b ~help:("enter", "next") [ "enter"; "tab" ];
      submit = b ~help:("enter", "submit") [ "enter" ];
    }
  in
  let file =
    {
      goto_top = disabled ~help:("g", "first") [ "g" ];
      goto_bottom = disabled ~help:("G", "last") [ "shift+g" ];
      page_up = disabled ~help:("pgup", "page up") [ "shift+k"; "page_up" ];
      page_down = disabled ~help:("pgdown", "page down") [ "shift+j"; "page_down" ];
      back = disabled ~help:("h", "back") [ "h"; "backspace"; "left"; "esc" ];
      select = disabled ~help:("enter", "select") [ "enter" ];
      up = disabled ~help:("up", "up") [ "up"; "k"; "ctrl+k"; "ctrl+p" ];
      down = disabled ~help:("down", "down") [ "down"; "j"; "ctrl+j"; "ctrl+n" ];
      open_ = disabled ~help:("enter", "open") [ "l"; "right"; "enter" ];
      close = disabled ~help:("esc", "close") [ "esc" ];
      prev = b ~help:("shift+tab", "back") [ "shift+tab" ];
      next = b ~help:("tab", "next") [ "tab" ];
      submit = b ~help:("enter", "submit") [ "enter" ];
    }
  in
  { quit; input; text; select; multi_select; confirm; note; file }
