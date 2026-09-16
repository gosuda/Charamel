type error =
  [ `Wrong_seed_length of int
  | `Wrong_word_count of int
  | `Unknown_word of string
  | `Bad_checksum ]

let pp_error ppf = function
  | `Wrong_seed_length n -> Format.fprintf ppf "melt: seed must be 32 bytes, got %d" n
  | `Wrong_word_count n -> Format.fprintf ppf "melt: mnemonic must be 24 words, got %d" n
  | `Unknown_word w -> Format.fprintf ppf "melt: %S is not a BIP-39 word" w
  | `Bad_checksum -> Format.fprintf ppf "melt: mnemonic checksum does not match"

let seed_bytes = 32
let word_count = 24
let bits_per_word = 11
let checksum_bytes = 1
let total_bytes = seed_bytes + checksum_bytes

(* [indices_of_bytes buf] reads [buf] as a big-endian bitstream, most significant bit
   first, and returns the 11-bit groups it holds in stream order. [buf]'s bit length
   must be a multiple of 11; every caller here passes the 264-bit
   checksum-plus-seed buffer, and 264 = 24 * 11, so the stream always divides evenly
   and no bits are ever dropped. *)
let indices_of_bytes buf =
  let n = Bytes.length buf in
  let acc = ref 0 and nbits = ref 0 in
  let out = ref [] in
  for i = 0 to n - 1 do
    acc := (!acc lsl 8) lor Char.code (Bytes.get buf i);
    nbits := !nbits + 8;
    while !nbits >= bits_per_word do
      let shift = !nbits - bits_per_word in
      out := ((!acc lsr shift) land 0x7FF) :: !out;
      nbits := shift;
      acc := !acc land ((1 lsl shift) - 1)
    done
  done;
  List.rev !out

(* The inverse of [indices_of_bytes]: packs 11-bit indices into a big-endian byte
   stream. Called only with exactly 24 indices (264 bits = 33 bytes), so the
   accumulator always empties exactly at the last byte. *)
let bytes_of_indices indices =
  let buf = Buffer.create total_bytes in
  let acc = ref 0 and nbits = ref 0 in
  List.iter
    (fun idx ->
      acc := (!acc lsl bits_per_word) lor idx;
      nbits := !nbits + bits_per_word;
      while !nbits >= 8 do
        let shift = !nbits - 8 in
        Buffer.add_char buf (Char.chr ((!acc lsr shift) land 0xFF));
        nbits := shift;
        acc := !acc land ((1 lsl shift) - 1)
      done)
    indices;
  Buffer.contents buf

let checksum_byte seed =
  (Digestif.SHA256.to_raw_string (Digestif.SHA256.digest_string seed)).[0]

let encode seed =
  let len = String.length seed in
  if len <> seed_bytes then Error (`Wrong_seed_length len)
  else begin
    let buf = Bytes.create total_bytes in
    Bytes.blit_string seed 0 buf 0 seed_bytes;
    Bytes.set buf seed_bytes (checksum_byte seed);
    let indices = indices_of_bytes buf in
    Ok (List.map (fun i -> Wordlist.words.(i)) indices)
  end

let decode words =
  let n = List.length words in
  if n <> word_count then Error (`Wrong_word_count n)
  else
    let rec to_indices acc = function
      | [] -> Ok (List.rev acc)
      | w :: rest -> (
          match Wordlist.find_index w with
          | None -> Error (`Unknown_word w)
          | Some i -> to_indices (i :: acc) rest)
    in
    let open Result.Syntax in
    let* indices = to_indices [] words in
    let buf = bytes_of_indices indices in
    let seed = String.sub buf 0 seed_bytes in
    let checksum = buf.[seed_bytes] in
    if Char.equal checksum (checksum_byte seed) then Ok seed else Error `Bad_checksum
