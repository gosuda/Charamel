type 'a t =
  | Const of 'a
  | Of_results of (Results.t -> 'a)
  | Of_results_async of (Results.t -> 'a)

let const value = Const value

let eval dynamic results =
  match dynamic with
  | Const value -> value
  | Of_results f | Of_results_async f -> f results
