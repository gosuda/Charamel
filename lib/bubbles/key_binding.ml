type t = { keys : Charm_tea.Key.t list; help : string * string; enabled : bool }

let v ?(help = ("", "")) ?(enabled = true) names =
  let keys =
    Stdlib.List.map
      (fun name ->
        match Charm_tea.Key.of_string name with
        | Ok key -> key
        | Error (`Msg message) -> invalid_arg (Fmt.str "invalid key %S: %s" name message))
      names
  in
  { keys; help; enabled }

let of_keys ?(help = ("", "")) ?(enabled = true) keys = { keys; help; enabled }
let enabled binding = binding.enabled && binding.keys <> []

let matches key binding =
  enabled binding
  && Stdlib.List.exists
       (fun candidate -> Charm_tea.Key.matches key candidate)
       binding.keys

let matches_any key bindings = Stdlib.List.exists (matches key) bindings
let set_enabled value binding = { binding with enabled = value }
let unbind _binding = { keys = []; help = ("", ""); enabled = false }
let set_keys keys binding = { binding with keys }
let set_help help binding = { binding with help }
