type 'a t = Const of 'a | Of_results of (Results.t -> 'a)

let const value = Const value

let eval dynamic results =
  match dynamic with Const value -> value | Of_results f -> f results
