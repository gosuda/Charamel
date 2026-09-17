(** Huh navigation key bindings. *)

type binding = Charamel_bubbles.Key_binding.t

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

val default : t
(** [default] is the standard Huh keymap. *)
