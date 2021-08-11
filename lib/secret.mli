module Key : sig
  type t

  val size : int
  (** [size] returns the size of [t] in bytes. *)

  val raw : t -> Cstruct.t
  (** [raw t] returns raw key bytes. *)

  val create : unit -> t

  val of_base64 : string -> (t, string) result
  (** [parse_key s] parses and decodes [s] from base64 string into a [t]. *)

  val to_base64 : t -> string
  (** [base64 t] encodes [t] into a base64 format string. *)

  val of_string : string -> (t, string) result
end

(** Represents an encrypted secret content. *)
type t

val of_base64 : string -> t
val to_base64 : t -> string
val nonce_size : int
val random_bytes : int -> Cstruct.t
val encrypt : key:Key.t -> contents:string -> t
val decrypt : key:Key.t -> contents:t -> (string, string) result
