module Key : sig
  type t

  val size : int
  (** [size] returns the size of {!type:t} in bytes. *)

  val raw : t -> Cstruct.t
  (** [raw t] returns raw key bytes. *)

  val create : unit -> t
  (** [create ()] is key {!type:t} whose size is {!val:size}. *)

  val of_string : string -> (t, string) result
  val of_base64 : string -> (t, string) result
  val to_base64 : t -> string
end

(** Represents a Base64 encoded encrypted secret data. The encryption/decryption
    cipher used is ChaCha20. *)
type t

val nonce_size : int
(** returns a nonce size (key size) for generating key which in turn can be used
    for as parameter for {!val:encrypt}/{!val:deccrypt}. *)

val random_bytes : int -> Cstruct.t
(** [random_bytes sz] returns random bytes of count [sz]. *)

val encrypt : Key.t -> string -> t
(** [encrypt key s] is {!type:t} of unencrypted data [s]. *)

val decrypt : Key.t -> t -> (string, string) result
(** [decrypt key t] returns a decrypted string value of [t]. *)

val decrypt_base64 : Key.t -> string -> (string, string) result
(** [decrypt_base64 key s] returns [Ok d] where [d] is the decrypted version of
    Base64 formatted [s]. It returns [Error e] if [s] is not a valid Base64
    string value. *)
