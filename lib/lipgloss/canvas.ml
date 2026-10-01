module Raster = Charamel_ansi.Raster

type t = { width : int; height : int; cells : Raster.cell array array }

let blank_grid width height =
  if width <= 0 || height <= 0 then [||]
  else Array.init height (fun _ -> Array.make width Raster.blank)

let create ~width ~height = { width; height; cells = blank_grid width height }

let resize t ~width ~height =
  let target = create ~width ~height in
  if width > 0 && height > 0 then
    Array.iteri
      (fun row source ->
        if row < height then
          Array.iteri
            (fun column cell ->
              if column < width then target.cells.(row).(column) <- cell)
            source)
      t.cells;
  target

let width t = t.width
let height t = t.height
let clear t = create ~width:t.width ~height:t.height

let in_bounds t ~x ~y =
  x >= 0 && y >= 0 && y < Array.length t.cells && x < Array.length t.cells.(y)

let cell_at t ~x ~y = if in_bounds t ~x ~y then t.cells.(y).(x) else Raster.blank

let set_cell t ~x ~y cell =
  if not (in_bounds t ~x ~y) then t
  else begin
    let cells = Array.copy t.cells in
    let row = Array.copy cells.(y) in
    row.(x) <- cell;
    cells.(y) <- row;
    { t with cells }
  end

let draw t ~x ~y text =
  let region_width = t.width - x and region_height = t.height - y in
  if String.equal text "" || region_width <= 0 || region_height <= 0 then t
  else begin
    let grid = Raster.layout ~width:region_width ~max_rows:region_height text in
    let cells = Array.copy t.cells in
    let put row column cell =
      if in_bounds t ~x:column ~y:row then begin
        let line = Array.copy cells.(row) in
        line.(column) <- cell;
        cells.(row) <- line
      end
    in
    Array.iteri
      (fun offset_y line ->
        let row = y + offset_y in
        let stop = Raster.last_nonblank line in
        if row < t.height then
          for column = 0 to stop do
            put row (x + column) line.(column)
          done)
      grid;
    { t with cells }
  end

let render t = Raster.to_string ~trim:true t.cells
