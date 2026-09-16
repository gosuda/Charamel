module Spring = struct
  (* The branch separator is the machine epsilon, [math.Nextafter (1.0,
     2.0) -. 1.0] in spring.go:82. *)
  let epsilon = Float.epsilon

  type t = {
    pos_pos_coef : float;
    pos_vel_coef : float;
    vel_pos_coef : float;
    vel_vel_coef : float;
  }

  let v ~delta_time ~angular_frequency ~damping_ratio =
    let angular_frequency = Float.max 0.0 angular_frequency in
    let damping_ratio = Float.max 0.0 damping_ratio in
    if angular_frequency < epsilon then
      { pos_pos_coef = 1.0; pos_vel_coef = 0.0; vel_pos_coef = 0.0; vel_vel_coef = 1.0 }
    else if damping_ratio > 1.0 +. epsilon then
      let za = -.angular_frequency *. damping_ratio in
      let zb =
        angular_frequency *. Float.sqrt ((damping_ratio *. damping_ratio) -. 1.0)
      in
      let z1 = za -. zb in
      let z2 = za +. zb in
      let e1 = Float.exp (z1 *. delta_time) in
      let e2 = Float.exp (z2 *. delta_time) in
      let inv_two_zb = 1.0 /. (2.0 *. zb) in
      let e1_over_two_zb = e1 *. inv_two_zb in
      let e2_over_two_zb = e2 *. inv_two_zb in
      let z1e1_over_two_zb = z1 *. e1_over_two_zb in
      let z2e2_over_two_zb = z2 *. e2_over_two_zb in
      {
        pos_pos_coef = (e1_over_two_zb *. z2) -. z2e2_over_two_zb +. e2;
        pos_vel_coef = -.e1_over_two_zb +. e2_over_two_zb;
        vel_pos_coef = (z1e1_over_two_zb -. z2e2_over_two_zb +. e2) *. z2;
        vel_vel_coef = -.z1e1_over_two_zb +. z2e2_over_two_zb;
      }
    else if damping_ratio < 1.0 -. epsilon then
      let omega_zeta = angular_frequency *. damping_ratio in
      let alpha =
        angular_frequency *. Float.sqrt (1.0 -. (damping_ratio *. damping_ratio))
      in
      let exp_term = Float.exp (-.omega_zeta *. delta_time) in
      let cos_term = Float.cos (alpha *. delta_time) in
      let sin_term = Float.sin (alpha *. delta_time) in
      let inv_alpha = 1.0 /. alpha in
      let exp_sin = exp_term *. sin_term in
      let exp_cos = exp_term *. cos_term in
      let exp_omega_zeta_sin_over_alpha =
        exp_term *. omega_zeta *. sin_term *. inv_alpha
      in
      {
        pos_pos_coef = exp_cos +. exp_omega_zeta_sin_over_alpha;
        pos_vel_coef = exp_sin *. inv_alpha;
        vel_pos_coef =
          (-.exp_sin *. alpha) -. (omega_zeta *. exp_omega_zeta_sin_over_alpha);
        vel_vel_coef = exp_cos -. exp_omega_zeta_sin_over_alpha;
      }
    else
      let exp_term = Float.exp (-.angular_frequency *. delta_time) in
      let time_exp = delta_time *. exp_term in
      let time_exp_freq = time_exp *. angular_frequency in
      {
        pos_pos_coef = time_exp_freq +. exp_term;
        pos_vel_coef = time_exp;
        vel_pos_coef = -.angular_frequency *. time_exp_freq;
        vel_vel_coef = -.time_exp_freq +. exp_term;
      }

  let update t ~pos ~vel ~target =
    let old_pos = pos -. target in
    let new_pos = (old_pos *. t.pos_pos_coef) +. (vel *. t.pos_vel_coef) +. target in
    let new_vel = (old_pos *. t.vel_pos_coef) +. (vel *. t.vel_vel_coef) in
    (new_pos, new_vel)
end

module Projectile = struct
  type vec = { x : float; y : float; z : float }
  type t = { pos : vec; vel : vec; acc : vec; delta_time : float }

  let v ~delta_time ~pos ~vel ~accel = { pos; vel; acc = accel; delta_time }

  let update t =
    let pos =
      {
        x = t.pos.x +. (t.vel.x *. t.delta_time);
        y = t.pos.y +. (t.vel.y *. t.delta_time);
        z = t.pos.z +. (t.vel.z *. t.delta_time);
      }
    in
    let vel =
      {
        x = t.vel.x +. (t.acc.x *. t.delta_time);
        y = t.vel.y +. (t.acc.y *. t.delta_time);
        z = t.vel.z +. (t.acc.z *. t.delta_time);
      }
    in
    { t with pos; vel }

  let pos t = t.pos
  let vel t = t.vel
  let gravity = { x = 0.0; y = -9.81; z = 0.0 }
  let terminal_gravity = { x = 0.0; y = 9.81; z = 0.0 }
end

let fps n =
  if n = 0 then invalid_arg "Charm_harmonica.fps: frame count must not be zero";
  1.0 /. float_of_int n
