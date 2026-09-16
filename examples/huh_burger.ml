let name_key = Charm_huh.Key.v "name"
let bun_key = Charm_huh.Key.v "bun"
let toppings_key = Charm_huh.Key.v "toppings"
let confirm_key = Charm_huh.Key.v "confirm"

let form =
  let open Charm_huh in
  Form.v
    [
      Group.v ~title:"Burger" ~description:"Build your burger"
        [
          Field.input ~title:(Dyn.const "Your name") ~placeholder:"Ada" name_key;
          Field.select ~title:(Dyn.const "Bun")
            ~options:
              (Dyn.const (Field.options_of_strings [ "brioche"; "sesame"; "pretzel" ]))
            bun_key;
          Field.multi_select ~title:(Dyn.const "Toppings")
            ~options:
              (Dyn.const (Field.options_of_strings [ "lettuce"; "tomato"; "onion" ]))
            toppings_key;
          Field.confirm ~title:(Dyn.const "Place order?") ~default:true confirm_key;
        ];
    ]

let print_results results =
  let get key default = Option.value (Charm_huh.Results.get key results) ~default in
  let name = get name_key "" in
  let bun = get bun_key "" in
  let toppings = get toppings_key [] in
  let confirmed = get confirm_key false in
  Fmt.pr "Name: %s@.Bun: %s@.Toppings: %a@.Confirmed: %b@." name bun (Fmt.list Fmt.string)
    toppings confirmed

let () =
  Eio_main.run (fun env ->
      match Charm_huh.run ~clock:env#clock form env with
      | Ok results -> print_results results
      | Error `Aborted -> Fmt.epr "Order cancelled.@."
      | Error `Timeout -> Fmt.epr "Order timed out.@.")
