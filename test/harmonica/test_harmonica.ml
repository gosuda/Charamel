open Charm_harmonica

let dt = fps 60
let f = Alcotest.float 1e-6
let pos_vel = Alcotest.pair f f

(* Spring.update against hand-computed evaluations of the spring.go
   coefficient formulas (dt = 1/60), cross-checked in closed form. *)

let spring_case name ~angular_frequency ~damping_ratio ~pos ~vel ~target expected =
  Alcotest.test_case name `Quick (fun () ->
      let s = Spring.v ~delta_time:dt ~angular_frequency ~damping_ratio in
      Alcotest.check pos_vel name expected (Spring.update s ~pos ~vel ~target))

let spring =
  ( "Spring.update matches the spring.go coefficients",
    [
      spring_case "over-damped, one step" ~angular_frequency:6.0 ~damping_ratio:1.5
        ~pos:0.0 ~vel:0.0 ~target:100.0
        (0.4531656955930998, 51.750134345383444);
      spring_case "critically damped, one step" ~angular_frequency:6.0 ~damping_ratio:1.0
        ~pos:0.0 ~vel:0.0 ~target:100.0
        (0.46788401604445085, 54.29024508215756);
      spring_case "under-damped, one step" ~angular_frequency:6.0 ~damping_ratio:0.2
        ~pos:0.0 ~vel:0.0 ~target:100.0
        (0.49298954054732746, 58.7178664830001);
      spring_case "under-damped, warm start" ~angular_frequency:6.0 ~damping_ratio:0.5
        ~pos:10.0 ~vel:(-3.0) ~target:100.0
        (10.387505333345601, 48.60171750647322);
      spring_case "no damping, one step" ~angular_frequency:6.0 ~damping_ratio:0.0
        ~pos:0.0 ~vel:0.0 ~target:100.0
        (0.49958347219741484, 59.900049988096896);
      spring_case "zero frequency is inert" ~angular_frequency:0.0 ~damping_ratio:1.0
        ~pos:10.0 ~vel:2.0 ~target:100.0 (10.0, 2.0);
      spring_case "negative frequency is clamped to inert" ~angular_frequency:(-5.0)
        ~damping_ratio:1.0 ~pos:10.0 ~vel:2.0 ~target:100.0 (10.0, 2.0);
      Alcotest.test_case "converges within ten seconds" `Quick (fun () ->
          let s = Spring.v ~delta_time:dt ~angular_frequency:6.0 ~damping_ratio:0.5 in
          let rec loop n pos vel =
            if n = 0 then (pos, vel)
            else
              let pos, vel = Spring.update s ~pos ~vel ~target:100.0 in
              loop (n - 1) pos vel
          in
          Alcotest.check
            (Alcotest.pair (Alcotest.float 1e-3) f)
            "settles on the target without residual velocity" (100.0, 0.0)
            (loop 600 0.0 0.0));
    ] )

(* Projectile.update against the projectile_test.go trajectories,
   tightened from the upstream 1e-2 threshold to 1e-6. *)

let check_vec name (expected : Projectile.vec) (actual : Projectile.vec) =
  Alcotest.(check f (name ^ " x") expected.Projectile.x actual.Projectile.x);
  Alcotest.(check f (name ^ " y") expected.Projectile.y actual.Projectile.y);
  Alcotest.(check f (name ^ " z") expected.Projectile.z actual.Projectile.z)

let advance secs t =
  let rec loop n t = if n = 0 then t else loop (n - 1) (Projectile.update t) in
  loop (secs * 60) t

let v3 x y z = { Projectile.x; y; z }

let projectile =
  ( "Projectile.update advances position before velocity",
    [
      Alcotest.test_case "one step: move by velocity, then accelerate" `Quick (fun () ->
          let t =
            Projectile.v ~delta_time:dt ~pos:(v3 8.0 20.0 0.0) ~vel:(v3 1.0 1.0 0.0)
              ~accel:(v3 0.0 9.81 0.0)
          in
          let t = Projectile.update t in
          check_vec "position"
            (v3 8.016666666666667 20.016666666666666 0.0)
            (Projectile.pos t);
          check_vec "velocity" (v3 1.0 1.1635 0.0) (Projectile.vel t));
      Alcotest.test_case "no acceleration keeps constant velocity" `Quick (fun () ->
          let t0 =
            Projectile.v ~delta_time:dt ~pos:(v3 0.0 0.0 0.0) ~vel:(v3 5.0 5.0 0.0)
              ~accel:(v3 0.0 0.0 0.0)
          in
          List.iter
            (fun secs ->
              let t = advance secs t0 in
              check_vec
                ("position after " ^ string_of_int secs ^ "s")
                (v3 (5.0 *. float_of_int secs) (5.0 *. float_of_int secs) 0.0)
                (Projectile.pos t);
              check_vec
                ("velocity after " ^ string_of_int secs ^ "s")
                (v3 5.0 5.0 0.0) (Projectile.vel t))
            [ 1; 2; 3; 4; 5; 6; 7 ]);
      Alcotest.test_case "terminal_gravity accelerates y by 9.81 per second" `Quick
        (fun () ->
          let t0 =
            Projectile.v ~delta_time:dt ~pos:(v3 0.0 0.0 0.0) ~vel:(v3 5.0 5.0 0.0)
              ~accel:Projectile.terminal_gravity
          in
          List.iter
            (fun (secs, y) ->
              check_vec
                ("position after " ^ string_of_int secs ^ "s")
                (v3 (5.0 *. float_of_int secs) y 0.0)
                (Projectile.pos (advance secs t0)))
            [
              (1, 9.82325);
              (2, 29.4565);
              (3, 58.89975);
              (4, 98.153);
              (5, 147.21625);
              (6, 206.0895);
              (7, 274.77275);
            ];
          check_vec "velocity after 7s"
            (v3 5.0 (5.0 +. (9.81 *. 7.0)) 0.0)
            (Projectile.vel (advance 7 t0)));
      Alcotest.test_case "gravity pulls y down" `Quick (fun () ->
          let t0 =
            Projectile.v ~delta_time:dt ~pos:(v3 0.0 0.0 0.0) ~vel:(v3 5.0 5.0 0.0)
              ~accel:Projectile.gravity
          in
          let t = advance 1 t0 in
          check_vec "position after 1s" (v3 5.0 0.17675 0.0) (Projectile.pos t);
          check_vec "velocity after 1s" (v3 5.0 (-4.81) 0.0) (Projectile.vel t));
    ] )

let fps_suite =
  ( "fps",
    [
      Alcotest.test_case "one second per frame at one fps" `Quick (fun () ->
          Alcotest.check (Alcotest.float 1e-15) "fps 1" 1.0 (fps 1));
      Alcotest.test_case "one sixtieth of a second at sixty fps" `Quick (fun () ->
          Alcotest.check (Alcotest.float 1e-15) "fps 60" (1.0 /. 60.0) (fps 60));
      Alcotest.test_case "zero is a programming error" `Quick (fun () ->
          Alcotest.check_raises "fps 0"
            (Invalid_argument "Charm_harmonica.fps: frame count must not be zero")
            (fun () -> ignore (fps 0)));
    ] )

let () = Alcotest.run "charm_harmonica" [ spring; projectile; fps_suite ]
