type match_ = { index : int; matched : int list; score : int }

let decode_scalars s =
  let length = String.length s in
  let rec loop byte_index rev_values =
    if byte_index >= length then Stdlib.List.rev rev_values
    else
      let decoded = String.get_utf_8_uchar s byte_index in
      let value = Uchar.utf_decode_uchar decoded in
      let step = max 1 (Uchar.utf_decode_length decoded) in
      loop (byte_index + step) (value :: rev_values)
  in
  loop 0 []

let simple_lower u =
  match Uucp.Case.Map.to_lower u with
  | `Self -> u
  | `Uchars (first :: _) -> first
  | `Uchars [] -> u

let equal_scalar left right = Uchar.equal (simple_lower left) (simple_lower right)

let is_separator u =
  match Uchar.to_int u with 0x2f | 0x2d | 0x5f | 0x20 | 0x2e | 0x5c -> true | _ -> false

let grapheme_map ~text ~scalar_count =
  let mapping = Array.make scalar_count 0 in
  let scalar_index = ref 0 in
  Stdlib.List.iteri
    (fun grapheme_index cluster ->
      let cluster_scalars = decode_scalars cluster in
      Stdlib.List.iter
        (fun _ ->
          if !scalar_index < scalar_count then begin
            mapping.(!scalar_index) <- grapheme_index;
            incr scalar_index
          end)
        cluster_scalars)
    (Charm_ansi.Width.graphemes text);
  mapping

let unique_graphemes ~text ~scalar_count scalar_indices =
  let mapping = grapheme_map ~text ~scalar_count in
  let rec loop previous rev_result = function
    | [] -> Stdlib.List.rev rev_result
    | scalar_index :: rest ->
        if scalar_index < 0 || scalar_index >= Array.length mapping then
          loop previous rev_result rest
        else
          let grapheme_index = mapping.(scalar_index) in
          if Some grapheme_index = previous then loop previous rev_result rest
          else loop (Some grapheme_index) (grapheme_index :: rev_result) rest
  in
  loop None [] scalar_indices

let score_candidate pattern_values ~index source =
  let candidate_values = decode_scalars source in
  let candidates = Array.of_list candidate_values in
  let pattern_length = Array.length pattern_values in
  let matched_rev = ref [] in
  let pattern_index = ref 0 in
  let best_score = ref (-1) in
  let best_index = ref (-1) in
  let total_score = ref 0 in
  let previous_match = ref (-1) in
  let previous_index = ref (-1) in
  let adjacent_bonus = ref 0 in
  let previous_value = ref Uchar.rep in
  for candidate_index = 0 to Array.length candidates - 1 do
    let candidate = candidates.(candidate_index) in
    if
      !pattern_index < pattern_length
      && equal_scalar candidate pattern_values.(!pattern_index)
    then begin
      let candidate_score = ref 0 in
      if candidate_index = 0 then candidate_score := !candidate_score + 10;
      if
        candidate_index <> 0
        && Uucp.Case.is_lower !previous_value
        && Uucp.Case.is_upper candidate
      then candidate_score := !candidate_score + 20;
      if candidate_index <> 0 && is_separator !previous_value then
        candidate_score := !candidate_score + 20;
      if !previous_match >= 0 then
        if !previous_match = !previous_index then begin
          let bonus = (2 * !adjacent_bonus) + 5 in
          candidate_score := !candidate_score + bonus;
          adjacent_bonus := bonus
        end
        else adjacent_bonus := 0;
      if !candidate_score > !best_score then begin
        best_score := !candidate_score;
        best_index := candidate_index
      end
    end;
    let next_candidate =
      if candidate_index + 1 < Array.length candidates then
        Some candidates.(candidate_index + 1)
      else None
    in
    let next_pattern =
      if !pattern_index + 1 < pattern_length then Some pattern_values.(!pattern_index + 1)
      else None
    in
    let commit =
      match (next_candidate, next_pattern) with
      | None, _ -> true
      | Some candidate, Some pattern -> equal_scalar candidate pattern
      | Some _, None -> false
    in
    if commit && !best_index >= 0 then begin
      if !matched_rev = [] then best_score := !best_score + max (-5 * !best_index) (-15);
      total_score := !total_score + !best_score;
      matched_rev := !best_index :: !matched_rev;
      previous_match := !best_index;
      pattern_index := !pattern_index + 1;
      best_score := -1;
      best_index := -1
    end;
    previous_index := candidate_index;
    previous_value := candidate
  done;
  let matched_scalar = Stdlib.List.rev !matched_rev in
  let total_score =
    !total_score + (Stdlib.List.length matched_scalar - Array.length candidates)
  in
  if !pattern_index = pattern_length then
    Some
      {
        index;
        matched =
          unique_graphemes ~text:source ~scalar_count:(Array.length candidates)
            matched_scalar;
        score = total_score;
      }
  else None

let find_unsorted ~pattern candidates =
  let pattern_values = Array.of_list (decode_scalars pattern) in
  if Array.length pattern_values = 0 then []
  else
    let rec loop index rev_matches = function
      | [] -> Stdlib.List.rev rev_matches
      | candidate :: rest ->
          let rev_matches =
            match score_candidate pattern_values ~index candidate with
            | None -> rev_matches
            | Some match_ -> match_ :: rev_matches
          in
          loop (index + 1) rev_matches rest
    in
    loop 0 [] candidates

let find ~pattern candidates =
  Stdlib.List.stable_sort
    (fun left right -> Int.compare right.score left.score)
    (find_unsorted ~pattern candidates)
