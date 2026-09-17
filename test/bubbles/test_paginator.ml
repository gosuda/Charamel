module Paginator = Charamel_bubbles.Paginator

let total_pages_and_bounds () =
  let paginator = Paginator.v ~per_page:5 ~total_pages:1 () in
  let paginator = Paginator.set_total_pages ~items:11 paginator in
  Alcotest.(check int) "ceil total pages" 3 (Paginator.total_pages paginator);
  let paginator = Paginator.set_page 2 paginator in
  Alcotest.(check bool) "last page" true (Paginator.on_last_page paginator);
  Alcotest.(check int)
    "last page item count" 1
    (Paginator.items_on_page ~total:11 paginator);
  Alcotest.(check (pair int int))
    "slice bounds" (10, 11)
    (Paginator.slice_bounds ~length:11 paginator);
  let unchanged = Paginator.set_total_pages ~items:(-10) paginator in
  Alcotest.(check int) "negative item count unchanged" 3 (Paginator.total_pages unchanged)

let navigation_clamps () =
  let paginator = Paginator.v ~total_pages:2 () in
  let paginator = Paginator.prev_page paginator in
  Alcotest.(check int) "prev at first" 0 (Paginator.page paginator);
  let paginator = Paginator.next_page paginator in
  Alcotest.(check int) "next from first" 1 (Paginator.page paginator);
  let paginator = Paginator.next_page paginator in
  Alcotest.(check int) "next at last" 1 (Paginator.page paginator);
  Alcotest.(check bool) "first false" false (Paginator.on_first_page paginator)

let views_and_keys () =
  let paginator = Paginator.v ~kind:Paginator.Dots ~total_pages:3 () in
  Alcotest.(check string) "dot view" "•○○" (Paginator.view paginator);
  let paginator, _ = Paginator.update Paginator.Next_page paginator in
  Alcotest.(check string) "next dot view" "○•○" (Paginator.view paginator);
  let key = Charamel_tea.Key.v Charamel_tea.Key.Left in
  match Paginator.key paginator key with
  | Some Paginator.Prev_page -> ()
  | _ -> Alcotest.fail "left key not bound"

let cases =
  [
    Alcotest.test_case "total pages and bounds" `Quick total_pages_and_bounds;
    Alcotest.test_case "navigation clamps" `Quick navigation_clamps;
    Alcotest.test_case "views and keys" `Quick views_and_keys;
  ]
