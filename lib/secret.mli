module Key : sig
  type t

  val raw : t -> Cstruct.t
  (** [raw t] returns raw key bytes. *)

  val create : int -> t
  (** [create sz] is key {!type:t} whose size [sz] . *)

  val of_string : string -> t
  val of_base64 : string -> (t, string) result
  val to_base64 : t -> string
end

val nonce_size : int
(** returns a nonce size (key size) for generating key which in turn can be used
    for as parameter for {!val:encrypt}/{!val:deccrypt}. *)

val random_bytes : int -> Cstruct.t
(** [random_bytes sz] returns random bytes of count [sz]. *)

val encrypt_base64 : Key.t -> string -> string
(** [encrypt key s] is {!type:t} of encrypted data [s]. *)

val decrypt_base64 : Key.t -> string -> (string, string) result
(** [decrypt_base64 key s] returns [Ok d] where [d] is the decrypted version of
    Base64 formatted [s]. It returns [Error e] if [s] is not a valid Base64
    string value or [decrypt_base64] is unable to decrypt [s]. *)
