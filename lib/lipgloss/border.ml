type t = {
  top : string;
  bottom : string;
  left : string;
  right : string;
  top_left : string;
  top_right : string;
  bottom_left : string;
  bottom_right : string;
  middle_left : string;
  middle_right : string;
  middle : string;
  middle_top : string;
  middle_bottom : string;
}

let normal =
  {
    top = "─";
    bottom = "─";
    left = "│";
    right = "│";
    top_left = "┌";
    top_right = "┐";
    bottom_left = "└";
    bottom_right = "┘";
    middle_left = "├";
    middle_right = "┤";
    middle = "┼";
    middle_top = "┬";
    middle_bottom = "┴";
  }

let rounded =
  { normal with top_left = "╭"; top_right = "╮"; bottom_left = "╰"; bottom_right = "╯" }

let block =
  {
    top = "█";
    bottom = "█";
    left = "█";
    right = "█";
    top_left = "█";
    top_right = "█";
    bottom_left = "█";
    bottom_right = "█";
    middle_left = "█";
    middle_right = "█";
    middle = "█";
    middle_top = "█";
    middle_bottom = "█";
  }

let outer_half_block =
  {
    top = "▀";
    bottom = "▄";
    left = "▌";
    right = "▐";
    top_left = "▛";
    top_right = "▜";
    bottom_left = "▙";
    bottom_right = "▟";
    middle_left = "";
    middle_right = "";
    middle = "";
    middle_top = "";
    middle_bottom = "";
  }

let inner_half_block =
  {
    top = "▄";
    bottom = "▀";
    left = "▐";
    right = "▌";
    top_left = "▗";
    top_right = "▖";
    bottom_left = "▝";
    bottom_right = "▘";
    middle_left = "";
    middle_right = "";
    middle = "";
    middle_top = "";
    middle_bottom = "";
  }

let thick =
  {
    top = "━";
    bottom = "━";
    left = "┃";
    right = "┃";
    top_left = "┏";
    top_right = "┓";
    bottom_left = "┗";
    bottom_right = "┛";
    middle_left = "┣";
    middle_right = "┫";
    middle = "╋";
    middle_top = "┳";
    middle_bottom = "┻";
  }

let double =
  {
    top = "═";
    bottom = "═";
    left = "║";
    right = "║";
    top_left = "╔";
    top_right = "╗";
    bottom_left = "╚";
    bottom_right = "╝";
    middle_left = "╠";
    middle_right = "╣";
    middle = "╬";
    middle_top = "╦";
    middle_bottom = "╩";
  }

let hidden =
  {
    top = " ";
    bottom = " ";
    left = " ";
    right = " ";
    top_left = " ";
    top_right = " ";
    bottom_left = " ";
    bottom_right = " ";
    middle_left = " ";
    middle_right = " ";
    middle = " ";
    middle_top = " ";
    middle_bottom = " ";
  }

let markdown =
  {
    top = "-";
    bottom = "-";
    left = "|";
    right = "|";
    top_left = "|";
    top_right = "|";
    bottom_left = "|";
    bottom_right = "|";
    middle_left = "|";
    middle_right = "|";
    middle = "|";
    middle_top = "|";
    middle_bottom = "|";
  }

let ascii =
  {
    top = "-";
    bottom = "-";
    left = "|";
    right = "|";
    top_left = "+";
    top_right = "+";
    bottom_left = "+";
    bottom_right = "+";
    middle_left = "+";
    middle_right = "+";
    middle = "+";
    middle_top = "+";
    middle_bottom = "+";
  }

let none =
  {
    top = "";
    bottom = "";
    left = "";
    right = "";
    top_left = "";
    top_right = "";
    bottom_left = "";
    bottom_right = "";
    middle_left = "";
    middle_right = "";
    middle = "";
    middle_top = "";
    middle_bottom = "";
  }

let max_rune_width s =
  Stdlib.List.fold_left
    (fun m g -> max m (Charm_ansi.Width.grapheme_width g))
    0
    (Charm_ansi.Width.graphemes s)

let edge_size a b c = max (max_rune_width a) (max (max_rune_width b) (max_rune_width c))
let top_size b = edge_size b.top_left b.top b.top_right
let right_size b = edge_size b.top_right b.right b.bottom_right
let bottom_size b = edge_size b.bottom_left b.bottom b.bottom_right
let left_size b = edge_size b.top_left b.left b.bottom_left
