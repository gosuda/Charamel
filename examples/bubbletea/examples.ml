type t = {
  name : string;
  main : unit -> unit Lwt.t;
  smoke : unit -> (string * string) list;
}

let all =
  [
    {
      name = "altscreen-toggle";
      main = Altscreen_toggle.main;
      smoke = Altscreen_toggle.smoke;
    };
    { name = "autocomplete"; main = Autocomplete.main; smoke = Autocomplete.smoke };
    { name = "canvas"; main = Canvas.main; smoke = Canvas.smoke };
    { name = "capability"; main = Capability.main; smoke = Capability.smoke };
    { name = "cellbuffer"; main = Cellbuffer.main; smoke = Cellbuffer.smoke };
    { name = "chat"; main = Chat.main; smoke = Chat.smoke };
    { name = "clickable"; main = Clickable.main; smoke = Clickable.smoke };
    { name = "colorprofile"; main = Colorprofile.main; smoke = Colorprofile.smoke };
    {
      name = "composable-views";
      main = Composable_views.main;
      smoke = Composable_views.smoke;
    };
    { name = "cursor-style"; main = Cursor_style.main; smoke = Cursor_style.smoke };
    { name = "debounce"; main = Debounce.main; smoke = Debounce.smoke };
    { name = "doom-fire"; main = Doom_fire.main; smoke = Doom_fire.smoke };
    {
      name = "dynamic-textarea";
      main = Dynamic_textarea.main;
      smoke = Dynamic_textarea.smoke;
    };
    { name = "exec"; main = Exec.main; smoke = Exec.smoke };
    { name = "eyes"; main = Eyes.main; smoke = Eyes.smoke };
    { name = "file-picker"; main = File_picker.main; smoke = File_picker.smoke };
    { name = "focus-blur"; main = Focus_blur.main; smoke = Focus_blur.smoke };
    { name = "fullscreen"; main = Fullscreen.main; smoke = Fullscreen.smoke };
    { name = "glamour"; main = Glamour.main; smoke = Glamour.smoke };
    { name = "help"; main = Help.main; smoke = Help.smoke };
    { name = "http"; main = Http.main; smoke = Http.smoke };
    { name = "isbn-form"; main = Isbn_form.main; smoke = Isbn_form.smoke };
    {
      name = "keyboard-enhancements";
      main = Keyboard_enhancements.main;
      smoke = Keyboard_enhancements.smoke;
    };
    { name = "list-default"; main = List_default.main; smoke = List_default.smoke };
    { name = "list-fancy"; main = List_fancy.main; smoke = List_fancy.smoke };
    { name = "list-simple"; main = List_simple.main; smoke = List_simple.smoke };
    { name = "mouse"; main = Mouse.main; smoke = Mouse.smoke };
    {
      name = "package-manager";
      main = Package_manager.main;
      smoke = Package_manager.smoke;
    };
    { name = "pager"; main = Pager.main; smoke = Pager.smoke };
    { name = "paginator"; main = Paginator.main; smoke = Paginator.smoke };
    { name = "pipe"; main = Pipe.main; smoke = Pipe.smoke };
    { name = "prevent-quit"; main = Prevent_quit.main; smoke = Prevent_quit.smoke };
    { name = "print-key"; main = Print_key.main; smoke = Print_key.smoke };
    {
      name = "progress-animated";
      main = Progress_animated.main;
      smoke = Progress_animated.smoke;
    };
    { name = "progress-bar"; main = Progress_bar.main; smoke = Progress_bar.smoke };
    {
      name = "progress-download";
      main = Progress_download.main;
      smoke = Progress_download.smoke;
    };
    {
      name = "progress-static";
      main = Progress_static.main;
      smoke = Progress_static.smoke;
    };
    { name = "query-term"; main = Query_term.main; smoke = Query_term.smoke };
    { name = "realtime"; main = Realtime.main; smoke = Realtime.smoke };
    { name = "result"; main = Result.main; smoke = Result.smoke };
    { name = "send-msg"; main = Send_msg.main; smoke = Send_msg.smoke };
    { name = "sequence"; main = Sequence.main; smoke = Sequence.smoke };
    {
      name = "set-terminal-color";
      main = Set_terminal_color.main;
      smoke = Set_terminal_color.smoke;
    };
    {
      name = "set-window-title";
      main = Set_window_title.main;
      smoke = Set_window_title.smoke;
    };
    { name = "simple"; main = Simple.main; smoke = Simple.smoke };
    { name = "space"; main = Space.main; smoke = Space.smoke };
    { name = "spinner"; main = Spinner.main; smoke = Spinner.smoke };
    { name = "spinners"; main = Spinners.main; smoke = Spinners.smoke };
    { name = "splash"; main = Splash.main; smoke = Splash.smoke };
    { name = "split-editors"; main = Split_editors.main; smoke = Split_editors.smoke };
    { name = "stopwatch"; main = Stopwatch.main; smoke = Stopwatch.smoke };
    { name = "suspend"; main = Suspend.main; smoke = Suspend.smoke };
    { name = "table"; main = Table.main; smoke = Table.smoke };
    { name = "table-resize"; main = Table_resize.main; smoke = Table_resize.smoke };
    { name = "tabs"; main = Tabs.main; smoke = Tabs.smoke };
    { name = "textarea"; main = Textarea.main; smoke = Textarea.smoke };
    { name = "textinput"; main = Textinput.main; smoke = Textinput.smoke };
    { name = "textinputs"; main = Textinputs.main; smoke = Textinputs.smoke };
    { name = "timer"; main = Timer.main; smoke = Timer.smoke };
    {
      name = "tui-daemon-combo";
      main = Tui_daemon_combo.main;
      smoke = Tui_daemon_combo.smoke;
    };
    { name = "vanish"; main = Vanish.main; smoke = Vanish.smoke };
    { name = "views"; main = Views.main; smoke = Views.smoke };
    { name = "window-size"; main = Window_size.main; smoke = Window_size.smoke };
  ]

let find name = List.find_opt (fun example -> String.equal example.name name) all
