type t = {
  major : int;
  minor : int;
  patch : int;
  prerelease : string list;
  build : string list;
}

type version = t
type operator = Eq | Ne | Gt | Ge | Lt | Le

type atom =
  | Any
  | Compare of operator * t
  | Not_compare of operator * t
  | Range of t * t
  | Not_range of t * t

type constraint_ = atom list list

let errorf fmt = Fmt.str fmt
let is_digit c = c >= '0' && c <= '9'

let is_ident_char c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '-'

let valid_identifier s = String.length s > 0 && String.for_all is_ident_char s

let numeric_identifier s =
  valid_identifier s && String.for_all is_digit s && (String.length s = 1 || s.[0] <> '0')

let parse_uint text what =
  if String.length text = 0 then Error (errorf "missing %s" what)
  else if not (String.for_all is_digit text) then Error (errorf "invalid %s %S" what text)
  else if String.length text > 1 && text.[0] = '0' then
    Error (errorf "leading zero in %s %S" what text)
  else
    try Ok (int_of_string text)
    with Failure _ -> Error (errorf "%s is too large: %S" what text)

let split_nonempty separator text =
  let fields = String.split_on_char separator text in
  if List.exists (fun field -> field = "") fields then None else Some fields

let parse_identifiers ~what text =
  match split_nonempty '.' text with
  | None -> Error (errorf "invalid %s %S" what text)
  | Some fields ->
      let valid field =
        valid_identifier field
        && ((not (String.for_all is_digit field)) || numeric_identifier field)
      in
      if List.for_all valid fields then Ok fields
      else Error (errorf "invalid %s %S" what text)

let parse_core text =
  let text =
    if String.length text > 0 && (text.[0] = 'v' || text.[0] = 'V') then
      String.sub text 1 (String.length text - 1)
    else text
  in
  let core_and_build = String.split_on_char '+' text in
  if List.length core_and_build > 2 then
    Error (errorf "invalid build metadata in %S" text)
  else
    let core, build =
      match core_and_build with
      | [ core ] -> (core, [])
      | [ core; suffix ] -> (core, [ suffix ])
      | _ -> assert false
    in
    let build_result =
      if build = [] then Ok []
      else parse_identifiers ~what:"build metadata" (List.hd build)
    in
    match build_result with
    | Error _ as error -> error
    | Ok build -> (
        let core_and_pre = String.split_on_char '-' core in
        let core, prerelease =
          match core_and_pre with
          | [ core ] -> (core, [])
          | first :: rest -> (first, [ String.concat "-" rest ])
          | [] -> assert false
        in
        let prerelease_result =
          match prerelease with
          | [] -> Ok []
          | [ value ] -> parse_identifiers ~what:"prerelease" value
          | _ -> assert false
        in
        match prerelease_result with
        | Error _ as error -> error
        | Ok prerelease -> (
            match split_nonempty '.' core with
            | None -> Error (errorf "invalid version core %S" core)
            | Some [ major; minor; patch ] ->
                let open Result.Syntax in
                let* major = parse_uint major "major version" in
                let* minor = parse_uint minor "minor version" in
                let* patch = parse_uint patch "patch version" in
                Ok { major; minor; patch; prerelease; build }
            | Some _ ->
                Error (errorf "version %S must contain major, minor, and patch" text)))

let parse text =
  match parse_core text with
  | Ok value -> Ok value
  | Error message -> Error (`Msg message)

let compare_identifier a b =
  let a_numeric = String.for_all is_digit a in
  let b_numeric = String.for_all is_digit b in
  match (a_numeric, b_numeric) with
  | true, true ->
      let strip s =
        let i = ref 0 in
        while !i + 1 < String.length s && s.[!i] = '0' do
          incr i
        done;
        String.sub s !i (String.length s - !i)
      in
      let a, b = (strip a, strip b) in
      let by_length = Int.compare (String.length a) (String.length b) in
      if by_length <> 0 then by_length else String.compare a b
  | true, false -> -1
  | false, true -> 1
  | false, false -> String.compare a b

let compare_prerelease a b =
  let rec loop a b =
    match (a, b) with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | x :: xs, y :: ys ->
        let result = compare_identifier x y in
        if result = 0 then loop xs ys else result
  in
  loop a b

let compare a b =
  let result = Int.compare a.major b.major in
  if result <> 0 then result
  else
    let result = Int.compare a.minor b.minor in
    if result <> 0 then result
    else
      let result = Int.compare a.patch b.patch in
      if result <> 0 then result
      else
        match (a.prerelease, b.prerelease) with
        | [], [] -> 0
        | [], _ :: _ -> 1
        | _ :: _, [] -> -1
        | _ -> compare_prerelease a.prerelease b.prerelease

let equal a b = compare a b = 0

let to_string version =
  let core = Fmt.str "%d.%d.%d" version.major version.minor version.patch in
  let core =
    match version.prerelease with
    | [] -> core
    | values -> core ^ "-" ^ String.concat "." values
  in
  match version.build with [] -> core | values -> core ^ "+" ^ String.concat "." values

type pattern = { base : t; wildcard : [ `None | `Major | `Minor | `Patch ] }

let parse_pattern text =
  let text = String.trim text in
  let text =
    if String.length text > 0 && (text.[0] = 'v' || text.[0] = 'V') then
      String.sub text 1 (String.length text - 1)
    else text
  in
  if text = "" || text = "*" || text = "x" || text = "X" then
    Ok
      {
        base = { major = 0; minor = 0; patch = 0; prerelease = []; build = [] };
        wildcard = `Major;
      }
  else
    let core_without_build, build =
      match String.index_opt text '+' with
      | None -> (text, [])
      | Some index ->
          let core = String.sub text 0 index in
          let suffix = String.sub text (index + 1) (String.length text - index - 1) in
          (core, [ suffix ])
    in
    let core_part, suffix =
      match String.index_opt core_without_build '-' with
      | None -> (core_without_build, "")
      | Some index ->
          ( String.sub core_without_build 0 index,
            String.sub core_without_build index (String.length core_without_build - index)
          )
    in
    let prerelease =
      if suffix = "" then []
      else String.sub suffix 1 (String.length suffix - 1) |> String.split_on_char '.'
    in
    if
      List.exists (fun value -> value = "") prerelease
      || List.exists
           (fun value ->
             (not (valid_identifier value))
             || (String.for_all is_digit value && not (numeric_identifier value)))
           prerelease
    then Error (errorf "invalid prerelease in %S" text)
    else
      let pieces = String.split_on_char '.' core_part in
      if List.length pieces > 3 || List.exists (fun value -> value = "") pieces then
        Error (errorf "invalid version pattern %S" text)
      else
        let pieces = pieces @ List.init (3 - List.length pieces) (fun _ -> "*") in
        let wildcard_at =
          let rec first index = function
            | [] -> `None
            | piece :: _ when piece = "*" || piece = "x" || piece = "X" -> (
                match index with 0 -> `Major | 1 -> `Minor | _ -> `Patch)
            | _ :: rest -> first (index + 1) rest
          in
          first 0 pieces
        in
        let invalid_after_wildcard =
          let rec check seen = function
            | [] -> false
            | piece :: rest ->
                let wildcard = piece = "*" || piece = "x" || piece = "X" in
                if seen && not wildcard then true else check (seen || wildcard) rest
          in
          check false pieces
        in
        if invalid_after_wildcard then Error (errorf "invalid wildcard pattern %S" text)
        else
          let parse_component value =
            if value = "*" || value = "x" || value = "X" then Ok 0
            else parse_uint value "version component"
          in
          let open Result.Syntax in
          let* major = parse_component (List.nth pieces 0) in
          let* minor = parse_component (List.nth pieces 1) in
          let* patch = parse_component (List.nth pieces 2) in
          let* build =
            match build with
            | [] -> Ok []
            | [ value ] -> parse_identifiers ~what:"build metadata" value
            | _ -> assert false
          in
          Ok { base = { major; minor; patch; prerelease; build }; wildcard = wildcard_at }

let next_major version =
  { major = version.major + 1; minor = 0; patch = 0; prerelease = []; build = [] }

let next_minor version =
  {
    major = version.major;
    minor = version.minor + 1;
    patch = 0;
    prerelease = [];
    build = [];
  }

let next_patch version =
  {
    major = version.major;
    minor = version.minor;
    patch = version.patch + 1;
    prerelease = [];
    build = [];
  }

let lower_for wildcard base =
  match wildcard with
  | `Major -> { base with minor = 0; patch = 0; prerelease = []; build = [] }
  | `Minor -> { base with patch = 0; prerelease = []; build = [] }
  | `Patch | `None -> base

let pattern_upper pattern =
  match pattern.wildcard with
  | `Major -> next_major pattern.base
  | `Minor -> next_major pattern.base
  | `Patch -> next_minor pattern.base
  | `None -> next_patch pattern.base

let atom_for operator pattern =
  let base = lower_for pattern.wildcard pattern.base in
  let upper = pattern_upper pattern in
  match (pattern.wildcard, operator) with
  | `Major, (Eq | Ge | Le) -> Any
  | `Major, Ne -> Not_compare (Eq, base)
  | `Major, Gt -> Compare (Ge, next_major base)
  | `Major, Lt -> Compare (Lt, base)
  | `Minor, Eq -> Range (base, upper)
  | `Minor, Ne -> Not_range (base, upper)
  | `Minor, Ge -> Compare (Ge, base)
  | `Minor, Gt -> Compare (Ge, upper)
  | `Minor, Le -> Compare (Lt, upper)
  | `Minor, Lt -> Compare (Lt, base)
  | `Patch, Gt -> Compare (Ge, upper)
  | `Patch, Eq -> Range (base, upper)
  | `Patch, Ne -> Not_range (base, upper)
  | `Patch, Ge -> Compare (Ge, base)
  | `Patch, Le -> Compare (Lt, upper)
  | `Patch, Lt -> Compare (Lt, base)
  | `None, _ -> Compare (operator, base)

let parse_operator token =
  let operators =
    [ (">=", Ge); ("<=", Le); ("!=", Ne); (">", Gt); ("<", Lt); ("=", Eq) ]
  in
  let rec find = function
    | [] -> None
    | (prefix, operator) :: rest ->
        if
          String.length token >= String.length prefix
          && String.sub token 0 (String.length prefix) = prefix
        then
          Some
            ( operator,
              String.sub token (String.length prefix)
                (String.length token - String.length prefix) )
        else find rest
  in
  match find operators with Some value -> value | None -> (Eq, token)

let trim_ascii text =
  let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false in
  let left = ref 0 in
  let right = ref (String.length text) in
  while !left < !right && is_space text.[!left] do
    incr left
  done;
  while !right > !left && is_space text.[!right - 1] do
    decr right
  done;
  String.sub text !left (!right - !left)

let words text =
  text
  |> String.map (fun c -> if c = ',' then ' ' else c)
  |> trim_ascii |> String.split_on_char ' '
  |> List.filter (fun word -> word <> "")

let parse_atom token =
  if token = "" || token = "*" || token = "x" || token = "X" then Ok [ Any ]
  else
    let op, value = parse_operator token in
    if value = "" then Error (errorf "missing version after operator")
    else
      let prefix, kind =
        if String.length value > 0 && value.[0] = '~' then
          (String.sub value 1 (String.length value - 1), `Tilde)
        else if String.length value > 0 && value.[0] = '^' then
          (String.sub value 1 (String.length value - 1), `Caret)
        else (value, `Plain)
      in
      match parse_pattern prefix with
      | Error _ as error -> error
      | Ok pattern -> (
          match kind with
          | `Plain -> Ok (atom_for op pattern :: [])
          | `Tilde ->
              if pattern.wildcard = `Major then Ok [ Any ]
              else
                let lower = lower_for pattern.wildcard pattern.base in
                let upper =
                  match pattern.wildcard with
                  | `Minor -> next_major lower
                  | `Patch | `None -> next_minor lower
                  | `Major -> next_major lower
                in
                Ok [ Compare (Ge, lower); Compare (Lt, upper) ]
          | `Caret ->
              if pattern.wildcard = `Major then Ok [ Any ]
              else
                let lower = lower_for pattern.wildcard pattern.base in
                let upper =
                  match pattern.wildcard with
                  | `Minor -> next_major lower
                  | `Patch ->
                      if lower.major > 0 then next_major lower else next_minor lower
                  | `None ->
                      if lower.major > 0 then next_major lower
                      else if lower.minor > 0 then next_minor lower
                      else next_patch lower
                  | `Major -> next_major lower
                in
                Ok [ Compare (Ge, lower); Compare (Lt, upper) ])

let parse_hyphen words =
  let rec find acc = function
    | left :: "-" :: right :: rest -> Some (List.rev acc, left, right, rest)
    | value :: rest -> find (value :: acc) rest
    | [] -> None
  in
  find [] words

let parse_and text =
  let tokens = words text in
  match tokens with
  | [] -> Ok []
  | _ -> (
      match parse_hyphen tokens with
      | Some (before, left, right, after) -> (
          if before <> [] || after <> [] then
            Error (errorf "invalid hyphen range %S" text)
          else
            let open Result.Syntax in
            let* left = parse_pattern left in
            let* right = parse_pattern right in
            let low = lower_for left.wildcard left.base in
            match right.wildcard with
            | `None -> Ok [ Compare (Ge, low); Compare (Le, right.base) ]
            | _ ->
                let high = pattern_upper right in
                Ok [ Compare (Ge, low); Compare (Lt, high) ])
      | None ->
          let rec collect acc = function
            | [] -> Ok (List.rev acc)
            | token :: value :: rest
              when List.mem token [ "="; "!="; ">"; ">="; "<"; "<="; "~"; "^" ] ->
                let open Result.Syntax in
                let* atoms = parse_atom (token ^ value) in
                collect (List.rev_append atoms acc) rest
            | token :: rest ->
                let open Result.Syntax in
                let* atoms = parse_atom token in
                collect (List.rev_append atoms acc) rest
          in
          collect [] tokens)

let parse_constraint_core text =
  if String.contains text '|' then
    let chunks =
      let rec split acc start =
        match String.index_from_opt text start '|' with
        | None -> List.rev (String.sub text start (String.length text - start) :: acc)
        | Some index when index + 1 < String.length text && text.[index + 1] = '|' ->
            let chunk = String.sub text start (index - start) in
            split (chunk :: acc) (index + 2)
        | Some _ -> [ "" ]
      in
      split [] 0
    in
    if List.exists (fun chunk -> String.trim chunk = "") chunks then
      Error (errorf "invalid disjunction %S" text)
    else
      let open Result.Syntax in
      let* parsed =
        List.fold_left
          (fun acc chunk ->
            let* acc = acc in
            let* atoms = parse_and chunk in
            Ok (atoms :: acc))
          (Ok []) chunks
      in
      Ok (List.rev parsed)
  else
    let open Result.Syntax in
    let* atoms = parse_and text in
    Ok [ atoms ]

let parse_constraint text =
  match parse_constraint_core text with
  | Ok value -> Ok value
  | Error message -> Error (`Msg message)

let rec eval_atom atom version =
  match atom with
  | Any -> true
  | Compare (operator, expected) -> (
      let result = compare version expected in
      match operator with
      | Eq -> result = 0
      | Ne -> result <> 0
      | Gt -> result > 0
      | Ge -> result >= 0
      | Lt -> result < 0
      | Le -> result <= 0)
  | Not_compare (operator, expected) ->
      not (eval_atom (Compare (operator, expected)) version)
  | Range (low, high) -> compare version low >= 0 && compare version high < 0
  | Not_range (low, high) -> not (compare version low >= 0 && compare version high < 0)

let satisfies constraint_ version =
  let group_allows_prerelease atoms =
    List.exists
      (function
        | Compare (_, value) | Not_compare (_, value) -> value.prerelease <> []
        | Range (low, high) | Not_range (low, high) ->
            low.prerelease <> [] || high.prerelease <> []
        | Any -> false)
      atoms
  in
  List.exists
    (fun atoms ->
      (version.prerelease = [] || group_allows_prerelease atoms)
      && List.for_all (fun atom -> eval_atom atom version) atoms)
    constraint_

let check = satisfies
