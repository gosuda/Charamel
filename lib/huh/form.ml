open Lwt.Syntax
module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub
module Key = Charamel_tea.Key
module Event = Charamel_tea.Event
module Style = Charamel_lipgloss.Style

type state = Normal | Completed of Results.t | Aborted

type msg =
  | Key_press of Key.t
  | Paste of string
  | Field_msg of { group : int; field : int; msg : Field_msg.t }
  | Terminal of Event.t
  | Resize of { rows : int; cols : int }

type t = {
  groups : Group.t array;
  selected : int;
  state : state;
  results : Results.t;
  theme : Theme.t;
  styles : Styles.t;
  keymap : Keymap.t;
  layout : Layout.t;
  width : int;
  explicit_width : int option;
  height : int;
  explicit_height : int option;
  show_help : bool option;
  show_errors : bool option;
  env : Env.t option;
}

module Env = Env

let normal_state = function Normal -> true | Completed _ | Aborted -> false
let cmd_none = Cmd.none

let v ?(theme = Theme.charm) ?(keymap = Keymap.default) ?(layout = `Default) ?width
    ?height ?show_help ?show_errors groups =
  let initial_width = Option.value ~default:80 width in
  let initial_height = Option.value ~default:0 height in
  let group_width = Layout.group_width layout ~width:initial_width in
  let groups =
    groups |> Array.of_list
    |> Array.map (Group.set_size ~width:group_width ~height:initial_height)
  in
  {
    groups;
    selected = 0;
    state = Normal;
    results = Results.empty;
    theme;
    styles = theme ~is_dark:true;
    keymap;
    layout;
    width = initial_width;
    explicit_width = width;
    height = initial_height;
    explicit_height = height;
    show_help;
    show_errors;
    env = None;
  }

let selected_group t =
  if t.selected < 0 || t.selected >= Array.length t.groups then None
  else Some t.groups.(t.selected)

let default_position = { Field_impl.is_first = false; is_last = false }

let context_for t ~env ~group_index ~position =
  let group = t.groups.(group_index) in
  {
    Field_impl.styles = t.styles;
    keymap = t.keymap;
    width = Group.width group;
    height = Group.height group;
    position;
    results = t.results;
    env;
  }

let skip_for t ~env ~group_index field_index =
  let field = Group.field field_index t.groups.(group_index) in
  Field_impl.skip field (context_for t ~env ~group_index ~position:default_position)

let visible_fields t ~env group_index =
  Group.visible_indices
    ~skip:(fun field_index -> skip_for t ~env ~group_index field_index)
    t.groups.(group_index)

let field_position t ~env group_index field_index =
  let indices = visible_fields t ~env group_index in
  let first = match indices with index :: _ -> index | [] -> field_index in
  let last = match List.rev indices with index :: _ -> index | [] -> field_index in
  { Field_impl.is_first = field_index = first; is_last = field_index = last }

let context t ~env group_index field_index =
  context_for t ~env ~group_index
    ~position:(field_position t ~env group_index field_index)

let set_group t index group =
  if index < 0 || index >= Array.length t.groups then t
  else
    {
      t with
      groups = Array.mapi (fun i old -> if i = index then group else old) t.groups;
    }

let reevaluate_all t =
  match t.env with
  | None -> t
  | Some env ->
      let groups =
        Array.mapi
          (fun group_index (group : Group.t) ->
            let fields =
              Array.mapi
                (fun field_index field ->
                  Field_impl.reevaluate field (context t ~env group_index field_index))
                group.Group.fields
            in
            { group with fields })
          t.groups
      in
      { t with groups }

let commit_field t group_index field_index =
  if group_index < 0 || group_index >= Array.length t.groups then t
  else
    let group = t.groups.(group_index) in
    if field_index < 0 || field_index >= Group.field_count group then t
    else
      let field = Group.field field_index group in
      { t with results = Field_impl.commit field t.results }

let rec complete_results t results index =
  if index = Array.length t.groups then results
  else
    let group = t.groups.(index) in
    if Group.is_hidden ~results group then complete_results t results (index + 1)
    else
      let results =
        Array.fold_left
          (fun acc field -> Field_impl.commit field acc)
          results group.Group.fields
      in
      complete_results t results (index + 1)

let complete t =
  let results = complete_results t t.results 0 in
  { t with results; state = Completed results }

let first_field t group_index =
  Option.bind t.env (fun env ->
      Group.first_index
        ~skip:(fun index -> skip_for t ~env ~group_index index)
        t.groups.(group_index))

let focus_field t group_index field_index =
  match t.env with
  | None -> (t, cmd_none)
  | Some env ->
      let group = t.groups.(group_index) in
      let field = Group.field field_index group in
      let field, cmd = Field_impl.focus field (context t ~env group_index field_index) in
      let group =
        group
        |> Group.set_selected field_index
        |> Group.set_active true
        |> Group.set_field field_index field
      in
      ( set_group t group_index group,
        Cmd.map
          (fun msg -> Field_msg { group = group_index; field = field_index; msg })
          cmd )

let blur_field t group_index field_index =
  match t.env with
  | None -> t
  | Some env ->
      let group = t.groups.(group_index) in
      let field = Group.field field_index group in
      let field = Field_impl.blur field (context t ~env group_index field_index) in
      set_group t group_index (Group.set_field field_index field group)

let init_group t group_index =
  match t.env with
  | None -> (t, cmd_none)
  | Some env -> (
      let group = t.groups.(group_index) in
      match first_field t group_index with
      | None -> (set_group t group_index (Group.set_active false group), cmd_none)
      | Some field_index ->
          let initialized, commands =
            Array.mapi
              (fun index field ->
                let field, command =
                  Field_impl.init field (context t ~env group_index index)
                in
                ( field,
                  Cmd.map
                    (fun m -> Field_msg { group = group_index; field = index; msg = m })
                    command ))
              group.Group.fields
            |> Array.split
          in
          let group =
            {
              (Group.set_selected field_index (Group.set_active true group)) with
              fields = initialized;
            }
          in
          let t = set_group t group_index group in
          let t, focus_cmd = focus_field t group_index field_index in
          (t, Cmd.batch (focus_cmd :: Array.to_list commands)))

let init env t =
  let t =
    { t with env = Some env; results = Results.empty; state = Normal } |> reevaluate_all
  in
  let rec find index =
    if index = Array.length t.groups then None
    else if Group.is_hidden ~results:t.results t.groups.(index) then find (index + 1)
    else
      match first_field t index with
      | None -> find (index + 1)
      | Some _ ->
          let t = { t with selected = index } in
          Some (index, init_group t index)
  in
  match find 0 with
  | None ->
      let t = { t with state = Completed t.results } in
      (t, Cmd.none)
  | Some (_index, (t, command)) -> (t, Cmd.batch [ command; Cmd.query `Background ])

let errors t = match selected_group t with None -> [] | Some group -> Group.errors group
let field_error_free t group_index = Group.errors t.groups.(group_index) = []

let next_group t =
  let rec find index =
    if index = Array.length t.groups then None
    else if
      index > t.selected
      && (not (Group.is_hidden ~results:t.results t.groups.(index)))
      && Option.is_some (first_field t index)
    then Some index
    else find (index + 1)
  in
  match find 0 with
  | None -> (complete t, cmd_none)
  | Some group_index ->
      let t = blur_field t t.selected (Group.selected t.groups.(t.selected)) in
      let t = set_group t t.selected (Group.set_active false t.groups.(t.selected)) in
      let t = { t with selected = group_index } in
      init_group t group_index

let previous_group t =
  let rec find index =
    if index < 0 then None
    else if
      index < t.selected
      && (not (Group.is_hidden ~results:t.results t.groups.(index)))
      && Option.is_some (first_field t index)
    then Some index
    else find (index - 1)
  in
  match find (Array.length t.groups - 1) with
  | None -> (t, cmd_none)
  | Some group_index ->
      let old = t.groups.(t.selected) in
      let t =
        if Group.field_count old = 0 then t
        else blur_field t t.selected (Group.selected old)
      in
      let t = set_group t t.selected (Group.set_active false t.groups.(t.selected)) in
      let t = { t with selected = group_index } in
      init_group t group_index

let adjust_offset t group_index =
  match t.env with
  | None -> t
  | Some env ->
      let group = t.groups.(group_index) in
      let indices = visible_fields t ~env group_index in
      let line_count text = max 1 (List.length (String.split_on_char '\n' text)) in
      let separator_lines =
        max 1 (List.length (String.split_on_char '\n' t.styles.Styles.field_separator) - 1)
      in
      let rec positions before = function
        | [] -> (0, 1)
        | index :: rest ->
            let field = Group.field index group in
            let text =
              Field_impl.view field
                (context t ~env group_index index)
                ~focused:(index = Group.selected group)
            in
            let height = line_count text in
            if index = Group.selected group then (before, height)
            else positions (before + height + separator_lines) rest
      in
      let top, selected_height = positions 0 indices in
      let viewport = max 1 (Group.height group) in
      let offset =
        if top < Group.y_offset group then top
        else if top + selected_height > Group.y_offset group + viewport then
          top + selected_height - viewport
        else Group.y_offset group
      in
      set_group t group_index (Group.set_y_offset offset group)

let wrap_field_cmd group_index field_index cmd =
  Cmd.map
    (fun message -> Field_msg { group = group_index; field = field_index; msg = message })
    cmd

let move_within_group t group_index direction =
  let group = t.groups.(group_index) in
  let selected = Group.selected group in
  let next =
    match direction with
    | `Next -> (
        match t.env with
        | Some env ->
            Group.next_index
              ~skip:(fun i -> skip_for t ~env ~group_index i)
              group selected
        | None -> None)
    | `Prev -> (
        match t.env with
        | Some env ->
            Group.previous_index
              ~skip:(fun i -> skip_for t ~env ~group_index i)
              group selected
        | None -> None)
  in
  Option.bind next (fun field_index ->
      let t =
        match direction with `Next -> blur_field t group_index selected | `Prev -> t
      in
      let t, command = focus_field t group_index field_index in
      Some (adjust_offset t group_index, command))

let handle_outcome t group_index field_index outcome command =
  match outcome with
  | Field_impl.Stay -> (t, command)
  | Field_impl.Next -> (
      let t = commit_field t group_index field_index |> reevaluate_all in
      if not (field_error_free t group_index) then (t, command)
      else if Group.is_hidden ~results:t.results t.groups.(group_index) then
        let t, next = next_group t in
        (t, Cmd.batch [ command; next ])
      else
        match move_within_group t group_index `Next with
        | Some (t, movement) -> (t, Cmd.batch [ command; movement ])
        | None ->
            let t, next = next_group t in
            (t, Cmd.batch [ command; next ]))
  | Field_impl.Prev -> (
      match move_within_group t group_index `Prev with
      | Some (t, movement) -> (t, Cmd.batch [ command; movement ])
      | None ->
          if not (field_error_free t group_index) then (t, command)
          else
            let t, previous = previous_group t in
            (t, Cmd.batch [ command; previous ]))
  | Field_impl.Submit ->
      let t = commit_field t group_index field_index |> reevaluate_all in
      if not (field_error_free t group_index) then (t, command)
      else
        let t, next = next_group t in
        (t, Cmd.batch [ command; next ])

let dispatch_key t key =
  match (t.env, selected_group t) with
  | None, _ | _, None -> (t, cmd_none)
  | Some env, Some group ->
      let group_index = t.selected in
      let field_index = Group.selected group in
      let field = Group.field field_index group in
      let field, command, outcome =
        Field_impl.step_key field (context t ~env group_index field_index) key
      in
      let t = set_group t group_index (Group.set_field field_index field group) in
      let t = adjust_offset t group_index in
      handle_outcome t group_index field_index outcome
        (wrap_field_cmd group_index field_index command)

let dispatch_paste t text =
  match (t.env, selected_group t) with
  | None, _ | _, None -> (t, cmd_none)
  | Some env, Some group ->
      let group_index = t.selected in
      let field_index = Group.selected group in
      let field = Group.field field_index group in
      let field =
        Field_impl.step_paste field (context t ~env group_index field_index) text
      in
      let t = set_group t group_index (Group.set_field field_index field group) in
      (adjust_offset t group_index, cmd_none)

let dispatch_field_message t ~group_index ~field_index field_message =
  match t.env with
  | None -> (t, cmd_none)
  | Some env ->
      if group_index < 0 || group_index >= Array.length t.groups then (t, cmd_none)
      else
        let group = t.groups.(group_index) in
        if field_index < 0 || field_index >= Group.field_count group then (t, cmd_none)
        else
          let field = Group.field field_index group in
          let field, command =
            Field_impl.step_msg field
              (context t ~env group_index field_index)
              field_message
          in
          let t =
            set_group t group_index (Group.set_field field_index field group)
            |> reevaluate_all
          in
          (adjust_offset t group_index, wrap_field_cmd group_index field_index command)

let is_dark_color color =
  match Charamel_ansi.Color.to_rgb color with
  | None -> true
  | Some (red, green, blue) ->
      (0.299 *. float red) +. (0.587 *. float green) +. (0.114 *. float blue) < 127.5

let raw_group_height t group_index =
  match t.env with
  | None -> 0
  | Some env ->
      let group = t.groups.(group_index) in
      if Group.is_hidden ~results:t.results group then 0
      else
        let group =
          Group.set_size ~width:(Group.width group)
            ~height:(max 1 (Group.height group))
            group
        in
        let t = set_group t group_index group in
        let indices = visible_fields t ~env group_index in
        let content =
          match Group.focused_field group with
          | Some field when Field_impl.zoom field && group.Group.active ->
              let field_index = Group.selected group in
              Field_impl.view field (context t ~env group_index field_index) ~focused:true
          | _ ->
              indices
              |> List.map (fun field_index ->
                  let field = Group.field field_index group in
                  ( field_index,
                    Field_impl.view field
                      (context t ~env group_index field_index)
                      ~focused:(group.Group.active && field_index = Group.selected group)
                  ))
              |> Group.content_lines ~separator:t.styles.Styles.field_separator
        in
        let title =
          if group.Group.title = "" then ""
          else Style.render t.styles.Styles.group_title group.Group.title
        in
        let description =
          if group.Group.description = "" then ""
          else Style.render t.styles.Styles.group_description group.Group.description
        in
        let header =
          let text =
            String.concat "\n"
              (List.filter (fun value -> value <> "") [ title; description ])
          in
          if text = "" then ""
          else Charamel_ansi.Text.wrap ~width:(max 1 (Group.width group)) text
        in
        let errors = Group.errors group in
        let show_errors = Option.value ~default:group.Group.show_errors t.show_errors in
        let show_help = Option.value ~default:group.Group.show_help t.show_help in
        let footer_lines =
          if errors <> [] && show_errors then List.length errors
          else if errors = [] && show_help then 1
          else 0
        in
        let lines text =
          if text = "" then 0 else List.length (String.split_on_char '\n' text)
        in
        lines header + lines content + 1 + footer_lines

let set_size ~rows ~cols t =
  let width = max 1 (Option.value ~default:cols t.explicit_width) in
  let requested_height = Option.value ~default:(max 0 rows) t.explicit_height |> max 0 in
  let group_width = Layout.group_width t.layout ~width in
  let groups =
    Array.map (Group.set_size ~width:group_width ~height:requested_height) t.groups
  in
  let sized = { t with width; height = requested_height; groups } in
  let raw_height =
    Array.fold_left
      (fun acc index -> max acc (raw_group_height sized index))
      0
      (Array.init (Array.length groups) Fun.id)
  in
  let height =
    match t.explicit_height with
    | Some value -> max 0 value
    | None -> if rows <= 0 then raw_height else min rows raw_height
  in
  let resized =
    {
      sized with
      height;
      groups = Array.map (Group.set_size ~width:group_width ~height) groups;
    }
  in
  reevaluate_all resized

let set_dark dark t = { t with styles = t.theme ~is_dark:dark }
let key key = Some (Key_press key)
let paste text = Paste text

let update message t =
  if Option.is_none t.env then
    invalid_arg "Charamel_huh.Form.update: Form.init must be called before update"
  else if not (normal_state t.state) then (t, cmd_none)
  else
    match message with
    | Key_press key when Charamel_bubbles.Key_binding.matches key t.keymap.Keymap.quit ->
        ({ t with state = Aborted }, cmd_none)
    | Key_press key -> dispatch_key t key
    | Paste text -> dispatch_paste t text
    | Field_msg { group; field; msg } ->
        dispatch_field_message t ~group_index:group ~field_index:field msg
    | Terminal (Event.Background_color color) ->
        (set_dark (is_dark_color color) t, cmd_none)
    | Terminal _ -> (t, cmd_none)
    | Resize { rows; cols } -> (set_size ~rows ~cols t, cmd_none)

let split_lines text = String.split_on_char '\n' text

let take_lines count lines =
  let rec take remaining source acc =
    if remaining <= 0 then List.rev acc
    else
      match source with
      | [] -> List.rev acc
      | line :: rest -> take (remaining - 1) rest (line :: acc)
  in
  take count lines []

let drop_lines count lines =
  let rec drop remaining = function
    | [] -> []
    | _ :: rest as values -> if remaining <= 0 then values else drop (remaining - 1) rest
  in
  drop count lines

let trim_right text =
  let index = ref (String.length text - 1) in
  while !index >= 0 && (text.[!index] = ' ' || text.[!index] = '\t') do
    decr index
  done;
  String.sub text 0 (!index + 1)

let render_header t group =
  let title =
    if group.Group.title = "" then ""
    else Style.render t.styles.Styles.group_title group.Group.title
  in
  let description =
    if group.Group.description = "" then ""
    else Style.render t.styles.Styles.group_description group.Group.description
  in
  let text =
    String.concat "\n" (List.filter (fun value -> value <> "") [ title; description ])
  in
  if text = "" then ""
  else Charamel_ansi.Text.wrap ~width:(max 1 (Group.width group)) text

let render_group t group_index =
  let group = t.groups.(group_index) in
  match t.env with
  | None -> { Layout.index = group_index; header = ""; content = ""; footer = "" }
  | Some env ->
      let indices = visible_fields t ~env group_index in
      let views =
        List.map
          (fun field_index ->
            let field = Group.field field_index group in
            let focused = group.Group.active && field_index = Group.selected group in
            ( field_index,
              Field_impl.view field (context t ~env group_index field_index) ~focused ))
          indices
      in
      let content =
        match Group.focused_field group with
        | Some field when Field_impl.zoom field && group.Group.active ->
            let field_index = Group.selected group in
            Field_impl.view field (context t ~env group_index field_index) ~focused:true
        | _ -> Group.content_lines ~separator:t.styles.Styles.field_separator views
      in
      let header = render_header t group in
      let errors = Group.errors group in
      let show_errors = Option.value ~default:group.Group.show_errors t.show_errors in
      let show_help = Option.value ~default:group.Group.show_help t.show_help in
      let footer =
        if errors <> [] then
          if show_errors then
            errors
            |> List.map (fun error ->
                Style.render t.styles.Styles.focused.Styles.error_message error)
            |> String.concat "\n"
            |> Charamel_ansi.Text.wrap ~width:(max 1 (Group.width group))
          else ""
        else if show_help then
          match Group.focused_field group with
          | None -> ""
          | Some field ->
              let field_index = Group.selected group in
              let help =
                Charamel_bubbles.Help.v ~width:(Group.width group)
                  ~styles:t.styles.Styles.help ()
              in
              Charamel_bubbles.Help.short_view help
                (Field_impl.key_binds field (context t ~env group_index field_index))
        else ""
      in
      let content_lines = split_lines content in
      let header_lines = if header = "" then 0 else List.length (split_lines header) in
      let footer_lines = if footer = "" then 0 else List.length (split_lines footer) in
      let visible_height =
        if Group.height group <= 0 then List.length content_lines
        else max 1 (Group.height group - header_lines - footer_lines - 1)
      in
      let content_window =
        content_lines
        |> drop_lines (Group.y_offset group)
        |> take_lines visible_height |> String.concat "\n" |> trim_right
      in
      let footer = trim_right footer in
      let group_style text =
        if text = "" then "" else Style.render t.styles.Styles.group_base text
      in
      {
        Layout.index = group_index;
        header = group_style header;
        content = group_style content_window;
        footer = group_style footer;
      }

let view t =
  let items =
    Array.to_list (Array.mapi (fun index group -> (index, group)) t.groups)
    |> List.filter_map (fun (index, group) ->
        if Group.is_hidden ~results:t.results group then None
        else Some (render_group t index))
  in
  let rendered = Layout.view t.layout ~width:t.width ~selected:t.selected items in
  let rendered =
    if t.height > 0 then
      rendered |> split_lines |> take_lines t.height |> String.concat "\n"
    else rendered
  in
  Style.render t.styles.Styles.form_base rendered

let subscriptions t =
  if not (normal_state t.state) then Sub.none
  else
    let global =
      [
        Sub.key (fun key -> Key_press key);
        Sub.paste paste;
        Sub.terminal (fun event -> Terminal event);
        Sub.resize (fun ~rows ~cols -> Resize { rows; cols });
      ]
    in
    match (t.env, selected_group t) with
    | Some env, Some group when Group.field_count group > 0 ->
        let field_index = Group.selected group in
        let field = Group.field field_index group in
        let field_sub =
          Field_impl.subscriptions field (context t ~env t.selected field_index)
          |> Sub.map (fun message ->
              Field_msg { group = t.selected; field = field_index; msg = message })
        in
        Sub.batch (field_sub :: global)
    | _ -> Sub.batch global

let state t =
  match t.state with
  | Normal -> `Normal
  | Completed results -> `Completed results
  | Aborted -> `Aborted

let results t = complete_results t t.results 0

let run_accessible env ~out reader t =
  let rec field_loop group field_index results =
    if field_index = Group.field_count group then Lwt.return (group, results)
    else
      let field = Group.field field_index group in
      let context =
        {
          Field_impl.styles = t.styles;
          keymap = t.keymap;
          width = 80;
          height = 0;
          position = default_position;
          results;
          env;
        }
      in
      let field = Field_impl.reevaluate field context in
      let* field = Field_impl.run_accessible field context ~out reader in
      let results = Field_impl.commit field results in
      field_loop (Group.set_field field_index field group) (field_index + 1) results
  in
  let rec group_loop group_index results =
    if group_index = Array.length t.groups then Lwt.return results
    else
      let group = t.groups.(group_index) in
      if Group.is_hidden ~results group then group_loop (group_index + 1) results
      else
        let* _, results = field_loop group 0 results in
        group_loop (group_index + 1) results
  in
  group_loop 0 t.results
