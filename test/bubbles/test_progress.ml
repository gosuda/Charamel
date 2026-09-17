module Progress = Charamel_bubbles.Progress
module Color = Charamel_ansi.Color
module Text = Charamel_ansi.Text

let settles_to_target () =
  let progress =
    Progress.v ~width:10 ~show_percentage:false () |> Progress.set_percent 1.0
  in
  let progress = ref progress in
  for _ = 1 to 300 do
    let next, _ = Progress.update Progress.Frame !progress in
    progress := next
  done;
  Alcotest.(check bool) "spring settles" false (Progress.is_animating !progress);
  Alcotest.(check (float 0.001)) "target percent" 1.0 (Progress.percent !progress);
  Alcotest.(check int) "full bar width" 10 (Text.width (Progress.view !progress))

let static_bar () =
  let progress = Progress.v ~width:10 ~show_percentage:false () in
  let output = Progress.view_as 0.5 progress |> Text.strip in
  Alcotest.(check string) "half bar" "█████░░░░░" output;
  let output = Progress.view_as 0.0 progress |> Text.strip in
  Alcotest.(check string) "empty bar" "░░░░░░░░░░" output

let custom_gradient_and_percentage () =
  let progress =
    Progress.v ~width:10
      ~colors:[ Color.Rgb (255, 0, 0); Color.Rgb (0, 0, 255) ]
      ~show_percentage:true ()
  in
  let output = Progress.view_as 0.5 progress in
  Alcotest.(check bool) "percentage is present" true (String.contains output '%');
  Alcotest.(check int) "bar keeps configured width" 10 (Text.width output)

let cases =
  [
    Alcotest.test_case "spring convergence" `Quick settles_to_target;
    Alcotest.test_case "solid fill" `Quick static_bar;
    Alcotest.test_case "gradient and percentage" `Quick custom_gradient_and_percentage;
  ]
