type t = [ `Default | `Stack | `Columns of int | `Grid of int * int ]
type item = { index : int; header : string; content : string; footer : string }

let positive value = max 1 value

let group_width layout ~width =
  let width = max 1 width in
  match layout with
  | `Default | `Stack -> width
  | `Columns columns -> max 1 (width / positive columns)
  | `Grid (_, columns) -> max 1 (width / positive columns)

let nonempty_parts parts = List.filter (fun part -> part <> "") parts
let join_vertical parts = String.concat "\n" (nonempty_parts parts)
let join_vertical_spaced parts = String.concat "\n\n" (nonempty_parts parts)

let selected_item ~selected items =
  match List.find_opt (fun item -> item.index = selected) items with
  | Some item -> item
  | None -> (
      match items with
      | item :: _ -> item
      | [] -> { index = selected; header = ""; content = ""; footer = "" })

let view layout ~width:_width ~selected items =
  match items with
  | [] -> ""
  | _ ->
      let selected_item = selected_item ~selected items in
      let selected_page =
        match List.find_opt (fun item -> item.index = selected_item.index) items with
        | Some item -> item
        | None -> selected_item
      in
      let with_footer body = join_vertical_spaced [ body; selected_page.footer ] in
      let with_footer_plain body = join_vertical [ body; selected_page.footer ] in
      begin match layout with
      | `Default ->
          with_footer (join_vertical [ selected_page.header; selected_page.content ])
      | `Stack ->
          let body = join_vertical_spaced (List.map (fun item -> item.content) items) in
          with_footer body
      | `Columns columns ->
          let columns = positive columns in
          let selected_position =
            let rec find position = function
              | [] -> 0
              | item :: rest ->
                  if item.index = selected_item.index then position
                  else find (position + 1) rest
            in
            find 0 items
          in
          let segment = selected_position / columns in
          let contents =
            items
            |> List.mapi (fun position item -> (position, item))
            |> List.filter_map (fun (position, item) ->
                if position / columns = segment then Some item.content else None)
          in
          let joined =
            Charm_lipgloss.Layout.join_horizontal ~pos:Charm_lipgloss.Position.top
              contents
          in
          with_footer_plain (join_vertical [ selected_page.header; joined ])
      | `Grid (rows, columns) ->
          let rows = positive rows in
          let columns = positive columns in
          let page_size = rows * columns in
          let selected_position =
            let rec find position = function
              | [] -> 0
              | item :: rest ->
                  if item.index = selected_item.index then position
                  else find (position + 1) rest
            in
            find 0 items
          in
          let page = selected_position / page_size in
          let page_items =
            items
            |> List.mapi (fun position item -> (position, item))
            |> List.filter_map (fun (position, item) ->
                if position / page_size = page then Some item else None)
          in
          let rec chunks n values =
            match values with
            | [] -> []
            | _ ->
                let row, rest =
                  let rec take remaining acc values =
                    if remaining = 0 then (List.rev acc, values)
                    else
                      match values with
                      | [] -> (List.rev acc, [])
                      | value :: tail -> take (remaining - 1) (value :: acc) tail
                  in
                  take columns [] values
                in
                row :: chunks n rest
          in
          let rows =
            chunks columns page_items
            |> List.map (fun row ->
                Charm_lipgloss.Layout.join_horizontal ~pos:Charm_lipgloss.Position.top
                  (List.map (fun item -> item.content) row))
          in
          with_footer (String.concat "\n\n" rows)
      end
