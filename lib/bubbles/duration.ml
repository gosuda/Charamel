let microsecond = 1_000L
let millisecond = 1_000_000L
let second = 1_000_000_000L

let fraction rem digits =
  if rem = 0L then ""
  else
    let bytes = Bytes.make digits '0' in
    let value = ref rem in
    for index = digits - 1 downto 0 do
      let digit = Int64.to_int (Int64.rem !value 10L) in
      Bytes.set bytes index (Char.chr (Char.code '0' + digit));
      value := Int64.div !value 10L
    done;
    let length = ref digits in
    while !length > 0 && Bytes.get bytes (!length - 1) = '0' do
      decr length
    done;
    if !length = 0 then "" else "." ^ Bytes.to_string (Bytes.sub bytes 0 !length)

let to_string seconds =
  if not (Float.is_finite seconds) then invalid_arg "Duration.to_string: non-finite value";
  let nanos_float = Float.round (seconds *. 1_000_000_000.) in
  let int64_limit = 9.223372036854776e18 in
  if
    (not (Float.is_finite nanos_float))
    || nanos_float >= int64_limit || nanos_float < -.int64_limit
  then invalid_arg "Duration.to_string: value outside nanosecond range";
  let nanos = Int64.of_float nanos_float in
  let negative = nanos < 0L in
  let negative_magnitude = if nanos > 0L then Int64.neg nanos else nanos in
  let prefix = if negative then "-" else "" in
  if negative_magnitude = 0L then "0s"
  else if negative_magnitude > Int64.neg second then
    if negative_magnitude > Int64.neg microsecond then
      prefix ^ Int64.to_string (Int64.neg negative_magnitude) ^ "ns"
    else if negative_magnitude > Int64.neg millisecond then
      prefix
      ^ Int64.to_string (Int64.neg (Int64.div negative_magnitude microsecond))
      ^ fraction (Int64.neg (Int64.rem negative_magnitude microsecond)) 3
      ^ "µs"
    else
      prefix
      ^ Int64.to_string (Int64.neg (Int64.div negative_magnitude millisecond))
      ^ fraction (Int64.neg (Int64.rem negative_magnitude millisecond)) 6
      ^ "ms"
  else
    let whole_seconds = Int64.div negative_magnitude second in
    let seconds_part = Int64.rem whole_seconds 60L in
    let total_minutes = Int64.div whole_seconds 60L in
    let minutes_part = Int64.rem total_minutes 60L in
    let hours_part = Int64.div total_minutes 60L in
    let result = Buffer.create 32 in
    Buffer.add_string result prefix;
    if hours_part < 0L then begin
      Buffer.add_string result (Int64.to_string (Int64.neg hours_part));
      Buffer.add_char result 'h'
    end;
    if total_minutes < 0L then begin
      Buffer.add_string result (Int64.to_string (Int64.neg minutes_part));
      Buffer.add_char result 'm'
    end;
    Buffer.add_string result (Int64.to_string (Int64.neg seconds_part));
    Buffer.add_string result
      (fraction (Int64.neg (Int64.rem negative_magnitude second)) 9);
    Buffer.add_char result 's';
    Buffer.contents result
