module Path = Crush_core.Path

let check_string name expected actual =
  Alcotest.check Alcotest.string name expected actual

let check_bool name expected actual = Alcotest.check Alcotest.bool name expected actual

let check_option name expected actual =
  Alcotest.(check (option string)) name expected actual

let absolute_paths () =
  check_string "root stays root" "/" (Path.normalize "/");
  check_string "dot-dot clamps at root" "/" (Path.normalize "/../..");
  check_string "inner dot-dot resolves" "/a/c" (Path.normalize "/a/b/../c");
  check_string "clamped dot-dot leaves the root" "/" (Path.normalize "/a/../..");
  check_string "trailing slash is dropped" "/a/b" (Path.normalize "/a/b/");
  check_string "empty component is dropped" "/a/b" (Path.normalize "/a//b");
  check_string "dot component is dropped" "/a/b" (Path.normalize "/a/./b")

let relative_paths () =
  check_string "empty normalizes to dot" "." (Path.normalize "");
  check_string "dot normalizes to dot" "." (Path.normalize ".");
  check_string "lone dot-dot clamps to dot" "." (Path.normalize "..");
  check_string "paired dot-dot clamps to dot" "." (Path.normalize "../..");
  check_string "third dot-dot clamps to dot" "." (Path.normalize "../../..");
  check_string "relative dot is dropped" "a/b" (Path.normalize "a/./b");
  check_string "relative dot-dot resolves" "a/c" (Path.normalize "a/b/../c");
  check_string "relative path passes through" "a/b" (Path.normalize "a/b")

let relative_to_cwd () =
  check_string "empty resolves to cwd" "/proj" (Path.normalize ~cwd:"/proj" "");
  check_string "dot resolves to cwd" "/proj" (Path.normalize ~cwd:"/proj" ".");
  check_string "bare dot-dot clamps at root" "/" (Path.normalize ~cwd:"/a" "..");
  check_string "mid-path dot-dot resolves" "/p/a/c" (Path.normalize ~cwd:"/p" "a/b/../c");
  check_string "escape above cwd reaches root" "/" (Path.normalize ~cwd:"/a" "x/../../..");
  check_string "absolute path ignores cwd" "/x" (Path.normalize ~cwd:"/proj" "/x");
  check_string "trailing slash is dropped" "/a/b" (Path.normalize ~cwd:"/p" "/a/b/");
  check_string "relative cwd stays relative" "x/a/b/c"
    (Path.normalize ~cwd:"x/a/b" "c/../c")

let containment () =
  check_bool "equal paths are within" true (Path.within ~root:"/a" "/a");
  check_bool "child is within" true (Path.within ~root:"/a" "/a/b");
  check_bool "sibling prefix is not within" false (Path.within ~root:"/a" "/ab");
  check_bool "resolved child is within" true (Path.within ~root:"/a" "/a/b/../c");
  check_bool "clamped escape is not within" false (Path.within ~root:"/a" "/a/../../b");
  check_bool "root contains every absolute path" true (Path.within ~root:"/" "/x");
  check_bool "relative path is never within" false (Path.within ~root:"/a" "a/b");
  check_bool "relative path is never within root" false (Path.within ~root:"/" "a");
  check_bool "relative root contains nothing" false (Path.within ~root:"a" "/a/b");
  check_bool "empty path is never within" false (Path.within ~root:"/a" "")

let relativization () =
  check_option "child becomes a relative path" (Some "b/c")
    (Path.relative ~root:"/a" "/a/b/c");
  check_option "root itself is empty" (Some "") (Path.relative ~root:"/a" "/a");
  check_option "root slash relativizes" (Some "a.ml") (Path.relative ~root:"/" "/a.ml");
  check_option "outside the root has no relative form" None
    (Path.relative ~root:"/a" "/b/c")

let parents () =
  check_option "child has a parent" (Some "/a") (Path.parent "/a/b");
  check_option "parent drops dots" (Some "/a") (Path.parent "/a/b/../c");
  check_option "root has no parent" None (Path.parent "/");
  check_option "one component has no parent" None (Path.parent "a");
  check_option "dot has no parent" None (Path.parent ".")

(* The platform argument pins the Windows rules on any host; [Sys.win32] decides the same
   rules for production callers. *)
let windows_paths () =
  check_string "backslashes unify" "C:/a/b" (Path.normalize ~windows:true "C:\\a\\b");
  check_string "drive letter is upper-cased" "C:/a" (Path.normalize ~windows:true "c:/a");
  check_string "drive dot-dot clamps at the drive root" "C:/"
    (Path.normalize ~windows:true "C:/a/..");
  check_string "drive dot-dot above the root clamps" "C:/"
    (Path.normalize ~windows:true "C:/a/../..");
  check_string "drive-relative path keeps the drive" "C:/b"
    (Path.normalize ~windows:true ~cwd:"C:/a" "/b");
  check_string "file URI path keeps its drive" "C:/x"
    (Path.normalize ~windows:true ~cwd:"C:/proj" "/C:/x");
  check_bool "drive root is absolute" true (Path.is_absolute ~windows:true "C:/a");
  check_bool "bare drive is absolute" true (Path.is_absolute ~windows:true "C:");
  check_string "unc root is server and share" "//srv/share/a"
    (Path.normalize ~windows:true "//srv/share/a");
  check_string "unc share root keeps its slash" "//srv/share/"
    (Path.normalize ~windows:true "//srv/share");
  check_option "unc parent stops at the share" None
    (Path.parent ~windows:true "//srv/share/");
  check_bool "unc child is within" true
    (Path.within ~windows:true ~root:"//srv/share" "//srv/share/a");
  check_bool "unc sibling share is not within" false
    (Path.within ~windows:true ~root:"//srv/share" "//srv/other/a")

let windows_containment () =
  check_bool "drive root contains its paths" true
    (Path.within ~windows:true ~root:"C:/a" "C:/a/b");
  check_bool "another drive is not within" false
    (Path.within ~windows:true ~root:"C:/a" "D:/a/b");
  check_bool "drive root contains the bare root" true
    (Path.within ~windows:true ~root:"C:/" "C:/x");
  check_bool "sibling prefix is not within a drive" false
    (Path.within ~windows:true ~root:"C:/a" "C:/ab");
  check_option "drive parent stops at the drive root" None
    (Path.parent ~windows:true "C:/a");
  check_option "drive root has no parent" None (Path.parent ~windows:true "C:/")

let posix_paths () =
  check_bool "a drive letter is not absolute" false
    (Path.is_absolute ~windows:false "C:/a");
  check_string "a backslash is an ordinary character" "a\\b"
    (Path.normalize ~windows:false "a\\b");
  check_bool "a drive-named directory is within" true
    (Path.within ~windows:false ~root:"/C:" "/C:/a")

let cases =
  [
    Test_tools_test_support.case "absolute paths" `Quick absolute_paths;
    Test_tools_test_support.case "relative paths" `Quick relative_paths;
    Test_tools_test_support.case "relative to cwd" `Quick relative_to_cwd;
    Test_tools_test_support.case "containment" `Quick containment;
    Test_tools_test_support.case "relativization" `Quick relativization;
    Test_tools_test_support.case "parents" `Quick parents;
    Test_tools_test_support.case "windows paths" `Quick windows_paths;
    Test_tools_test_support.case "windows containment" `Quick windows_containment;
    Test_tools_test_support.case "posix paths" `Quick posix_paths;
  ]
