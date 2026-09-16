(** Spring and projectile motion.

    Pure updates advance motion by a fixed time step. *)

module Spring : sig
  type t
  (** The type for springs with a fixed frequency, damping ratio, and time step. *)

  val v : delta_time:float -> angular_frequency:float -> damping_ratio:float -> t
  (** [v ~delta_time ~angular_frequency ~damping_ratio] is a spring whose [update]
      advances motion by [delta_time] seconds.

      [delta_time] is the frame length in seconds, typically [fps n]. [angular_frequency]
      is the undamped angular frequency of the motion, which affects how fast it settles.
      A [damping_ratio] above [1.0] is over-damped. [1.0] is critically damped. Values
      below [1.0] are under-damped.

      Negative values of [angular_frequency] and [damping_ratio] are clamped to [0.0]. A
      [damping_ratio] of [0.0] oscillates without decay. An [angular_frequency] below
      machine epsilon yields an inert spring whose [update] returns its inputs unchanged.
  *)

  val update : t -> pos:float -> vel:float -> target:float -> float * float
  (** [update t ~pos ~vel ~target] is the new position and velocity after advancing one
      step from [pos] with velocity [vel] toward [target]. Positive damping and frequency
      converge on a fixed target. Zero damping permits sustained oscillation. *)
end

module Projectile : sig
  type vec = { x : float; y : float; z : float }
  (** [vec] is a three-dimensional point or vector. *)

  type t
  (** The type for a projectile under constant acceleration. *)

  val v : delta_time:float -> pos:vec -> vel:vec -> accel:vec -> t
  (** [v ~delta_time ~pos ~vel ~accel] is a projectile at [pos] moving with [vel] under
      constant acceleration [accel], advanced [delta_time] seconds per [update]. *)

  val update : t -> t
  (** [update t] is the projectile after one time step. Position advances with the current
      velocity before acceleration changes that velocity. *)

  val pos : t -> vec
  (** [pos t] is the position of [t]. *)

  val vel : t -> vec
  (** [vel t] is the velocity of [t]. *)

  val gravity : vec
  (** [gravity] is [{ x = 0.0; y = -9.81; z = 0.0 }], for coordinate systems whose origin
      is in a bottom corner with [y] pointing up. *)

  val terminal_gravity : vec
  (** [terminal_gravity] is [{ x = 0.0; y = 9.81; z = 0.0 }], for coordinate systems whose
      origin is in a top corner with [y] pointing down. *)
end

val fps : int -> float
(** [fps n] is [1.0 /. float_of_int n], the length in seconds of a frame of [n] frames per
    second, for use as [~delta_time]. Raises [Invalid_argument] when [n] is zero. *)
