let initialize = lazy (Mirage_crypto_rng_unix.initialize ())
let nonce_size = 12

let random_bytes size =
  Lazy.force initialize ;
  Mirage_crypto_rng.generate size

let ( let+ ) r f = Result.map f r

module Key = struct
  type t = Cstruct.t

  let size = 32

  external raw : t -> Cstruct.t = "%identity"

  let create () = random_bytes size

  let check_key_size s =
    if String.length s = size then Ok s
    else
      Error (Format.sprintf "Invalid key size: Key size must be %d bytes" size)

  let of_base64 s =
    match Base64.(decode ~pad:false ~alphabet:uri_safe_alphabet s) with
    | Ok s ->
        let+ s = check_key_size s in
        Cstruct.of_string s
    | Error (`Msg msg) -> Error msg

  let to_base64 t =
    Cstruct.to_string t
    |> Base64.(encode_string ~pad:false ~alphabet:uri_safe_alphabet)

  let of_string s =
    let+ s = check_key_size s in
    Cstruct.of_string s
end

type t = string (* t is a base64 encoded string. *)

let encrypt key contents =
  let key = Mirage_crypto.Chacha20.of_secret (Key.raw key) in
  let nonce = Mirage_crypto_rng.generate nonce_size in
  let encrypted =
    Mirage_crypto.Chacha20.authenticate_encrypt ~key ~nonce
      (Cstruct.of_string contents)
  in
  Cstruct.concat [nonce; encrypted]
  |> Cstruct.to_string
  |> Base64.(encode_string ~alphabet:uri_safe_alphabet)

let decrypt key (t : t) =
  let contents = Cstruct.of_string t in
  let key = Mirage_crypto.Chacha20.of_secret (Key.raw key) in
  let nonce = Cstruct.sub contents 0 nonce_size in
  Cstruct.sub contents nonce_size (Cstruct.length contents - nonce_size)
  |> Mirage_crypto.Chacha20.authenticate_decrypt ~key ~nonce
  |> function
  | Some s -> Ok (Cstruct.to_string s)
  | None -> Error "Unable to decrypt contents"

let decrypt_base64 key contents =
  try
    let key = Mirage_crypto.Chacha20.of_secret (Key.raw key) in
    let contents =
      Base64.(decode_exn ~alphabet:uri_safe_alphabet contents)
      |> Cstruct.of_string
    in
    let nonce = Cstruct.sub contents 0 nonce_size in
    Cstruct.sub contents nonce_size (Cstruct.length contents - nonce_size)
    |> Mirage_crypto.Chacha20.authenticate_decrypt ~key ~nonce
    |> function
    | Some s -> Ok (Cstruct.to_string s)
    | None -> Error "Unable to decrypt contents"
  with exn -> Error (Format.sprintf "%s" (Printexc.to_string exn))
