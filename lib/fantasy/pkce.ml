type t = { verifier : string; challenge : string }

let b64url = Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet

let verifier_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
  | _ -> false

let valid_verifier verifier =
  let length = String.length verifier in
  if length < 43 || length > 128 then false
  else
    let rec loop index =
      if index = length then true else verifier_char verifier.[index] && loop (index + 1)
    in
    loop 0

let of_verifier verifier =
  if not (valid_verifier verifier) then
    invalid_arg "PKCE verifier must contain 43 to 128 unreserved characters";
  let digest = Digestif.SHA256.digest_string verifier in
  let challenge = b64url (Digestif.SHA256.to_raw_string digest) in
  { verifier; challenge }

let default_rng n = Mirage_crypto_rng.generate n

let generate ?(rng = default_rng) () =
  let entropy = rng 96 in
  if String.length entropy <> 96 then invalid_arg "PKCE RNG must return 96 bytes";
  of_verifier (b64url entropy)
