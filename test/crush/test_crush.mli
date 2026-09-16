(** Alcotest driver for the [crush.core] and [crush.ui] suites.

    The driver contributes no cases of its own. It groups the case lists exported by the
    sibling modules into the suites {!suites} names, so the whole harness is exercised by
    one executable. The command-line cases launch the built [crush] executable as a child
    process; the MCP stdio cases re-enter this same executable in fixture mode through an
    environment variable read at module initialisation. *)

val suites : (string * unit Alcotest.test_case list) list
(** [suites] is every Crush case module paired with the suite name it is reported under,
    ordered from the pure leaf contracts (hashline, configuration, model catalog,
    authentication, session store, state file, todos, tool boundary) through the policy
    and context layers (permission, rules, skills, hooks, jobs, artifacts, compaction,
    advisor, agent loop), the protocol clients (LSP, MCP), the tool families, and finally
    the command-line surface and the UI bridge. *)
