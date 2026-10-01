type t = { id : string; name : string; base_url : string; models : Model.t list }

let with_provider provider models =
  List.map (fun (m : Model.t) -> { m with Model.provider }) models

let jsont =
  let open Jsont in
  Object.map ~kind:"provider" (fun id name api_endpoint models ->
      { id; name; base_url = api_endpoint; models = with_provider id models })
  |> Object.mem "id" string ~enc:(fun p -> p.id)
  |> Object.mem "name" string ~enc:(fun p -> p.name)
  |> Object.mem "api_endpoint" string ~dec_absent:"" ~enc:(fun p -> p.base_url)
  |> Object.mem "models"
       (list (Model.jsont ~unknown:`Skip))
       ~dec_absent:[]
       ~enc:(fun p -> p.models)
  |> Object.finish
