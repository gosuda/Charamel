(** Clocks: real time for a running program, simulated time for a scripted one.

    A {!type:clock} answers two questions a user interface asks: how long to wait, and
    what time it is now. The real clock {!val:lwt} sleeps with [Lwt_unix.sleep] and stamps
    with the monotonic [Mtime_clock.now], so it is immune to the wall clock being moved
    and to time zones; monotonic seconds since an unspecified origin is what a stamp means
    here, which is the right basis for a duration and the wrong one for a calendar line,
    so formatting a timestamp uses {!val:wall}.

    A simulated clock exists because [Lwt] has no first-party mock clock:
    [Charamel_tea.Test] drives a program's timers without waiting for them, and a test of
    a 30-second spinner must not take 30 seconds. {!val:create_virtual} returns the clock
    plus its [advance] function; sleeping on the clock registers a deadline, and advancing
    resolves every deadline that has passed in deadline order, oldest first,
    deterministically and independently of any scheduler. The clock never moves on its
    own: a program that sleeps forever on a virtual clock blocks until somebody advances
    it, which is exactly what makes the test repeatable.

    Both platforms behave alike here, which is the point: [Lwt_unix.sleep] and
    [Mtime_clock.now] are the same calls on Windows and on POSIX, so a test that runs
    against a virtual clock on Linux exercises the very code path a Windows user interface
    takes. *)

type clock
(** A clock: either the real one or a simulated one. *)

val lwt : clock
(** The real clock: [Lwt_unix.sleep] for waiting, monotonic [Mtime_clock.now] for stamps.
*)

type virtual_clock
(** A simulated clock, with an observable deadline set. *)

val create_virtual : unit -> virtual_clock * (float -> unit)
(** [create_virtual ()] is a simulated clock starting at second [0.] and its advance
    function. [advance seconds] moves the clock forward by [seconds] and resolves every
    sleeper whose deadline has now passed, in deadline order, so a test can watch a
    program react to its own timers. Advancing backwards is a programming error.

    The returned clock is shared: every {!sleep} and {!now} through {!of_virtual} sees the
    same single timeline.
    @raise Invalid_argument when [advance] is called with a negative duration. *)

val next_deadline : virtual_clock -> float option
(** [next_deadline clock] is the simulated time at which the earliest pending sleeper
    wants to wake, or [None] when nothing is waiting. A synchronous driver —
    [Charamel_tea.Test] steps a program with no scheduler running — uses this to answer
    "how far must I advance before any progress is possible", which is what makes such a
    driver deterministic rather than a guessing loop. *)

val of_virtual : virtual_clock -> clock
(** [of_virtual clock] is the simulated clock as a {!type:clock}. *)

val sleep : clock -> float -> unit Lwt.t
(** [sleep clock seconds] waits for [seconds]. On the real clock this yields to the event
    loop; on a simulated clock it registers a deadline and stays pending until an
    [advance] passes it. A non-positive duration yields once and then resolves on either
    clock, and cancelling the promise on a simulated clock drops the deadline. *)

val now : clock -> float
(** [now clock] is the clock's current reading in seconds since an unspecified origin:
    monotonic time for the real clock, simulated time for a virtual one. Two calls on a
    simulated clock only differ if somebody advanced it in between. *)

val wall : clock -> float
(** [wall clock] is the clock's reading on the calendar: POSIX seconds since the Unix
    epoch, the basis for a calendar line. The real clock reads the system wall clock,
    which can step when the system adjusts it; {!val:now} is the reading to use for a
    duration. A simulated clock reports its simulated seconds from the epoch, so a
    scripted test stamps deterministic calendar time starting at [1970-01-01 00:00:00]. *)
