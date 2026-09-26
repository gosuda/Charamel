(** Skate store and command tests. *)

type test = unit Alcotest_lwt.test_case
(** The type for a store test case. *)

val roundtrip_text : test
(** [roundtrip_text] checks UTF-8 storage and retrieval. *)

val roundtrip_binary : test
(** [roundtrip_binary] checks raw byte storage and retrieval. *)

val absent_db_get : test
(** [absent_db_get] checks the missing database error from [get]. *)

val absent_db_delete : test
(** [absent_db_delete] checks the missing database error from [delete]. *)

val absent_db_list : test
(** [absent_db_list] checks that listing a missing database is empty. *)

val absent_key_get : test
(** [absent_key_get] checks the missing key error from [get]. *)

val absent_key_delete : test
(** [absent_key_delete] checks the missing key error from [delete]. *)

val corrupt_file_get : test
(** [corrupt_file_get] checks that a malformed file is rejected by [get]. *)

val corrupt_file_list : test
(** [corrupt_file_list] checks that a malformed file is rejected by [list]. *)

val invalid_db_slash : test
(** [invalid_db_slash] checks slash rejection in database names. *)

val invalid_db_dotdot : test
(** [invalid_db_dotdot] checks dot-dot rejection in database names. *)

val invalid_db_empty : test
(** [invalid_db_empty] checks empty database rejection. *)

val delete_db : test
(** [delete_db] checks database removal. *)

val dbs_sorted : test
(** [dbs_sorted] checks database ordering after suffix removal. *)

val dbs_absent_root : test
(** [dbs_absent_root] checks listing before root creation. *)

val atomic_0600 : test
(** [atomic_0600] checks the private mode of a stored database. *)

val binary_not_utf8 : test
(** [binary_not_utf8] checks persisted binary classification. *)

val show_binary_flag : test
(** [show_binary_flag] checks the command's binary output mode. *)

val corrupt_file_set : test
(** [corrupt_file_set] checks that [set] preserves malformed files. *)

val corrupt_file_delete : test
(** [corrupt_file_delete] checks that [delete] preserves malformed files. *)

val cli_roundtrip : test
(** [cli_roundtrip] checks the command surface against the real executable. *)

val cli_absent_paths : test
(** [cli_absent_paths] checks command errors for absent keys and databases. *)

val cli_unknown_command : test
(** [cli_unknown_command] checks rejection of an unknown command. *)

val cases : unit Alcotest_lwt.test
(** [cases] is the complete Skate test suite. *)
