(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

(** [Summer] is a HTTP/1.1 server. *)

(** [header] represents a HTTP header, a tuple of (name * value) *)
type header = string * string

type bigstring = Lwt_bytes.t

(** [error] represents an error string *)
type error = string

(** [chunk_extension] is an optional component of a chunk. It is defined at
    https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.1 *)
type chunk_extension = {name: string; value: string option}

(** [accept_encoding] represents [Accept-Encoding] and [Content-Encoding] header
    values. https://datatracker.ietf.org/doc/html/rfc7231#section-5.3.4 *)
type encoding = {name: encoder_name; weight: float option}

and encoder_name =
  [ `Compress
    (** Compress - https://datatracker.ietf.org/doc/html/rfc7230#section-4.2.1 *)
  | `Deflate
    (** Deflate - https://datatracker.ietf.org/doc/html/rfc7230#section-4.2.2 *)
  | `Gzip
    (** Gzip - https://datatracker.ietf.org/doc/html/rfc7230#section-4.2.3 *)
  | `Br  (** Br (Brotli) - https://datatracker.ietf.org/doc/html/rfc7932 *)
  | `Any
    (** Represented by '*': The asterisk "*" symbol in an Accept-Encoding field
        matches any available content-coding not explicitly listed in the header
        field. *)
  | `None
    (** Represented by empty ("Accept-Encoding: ") encoding header value. *)
  | `Other of string
    (** Any other encoding - possibly a custom one - not specified by the HTTP
        RFC 7230 or 7231 or 7932. *) ]

module Request : sig
  type t

  type meth =
    [ `GET
    | `HEAD
    | `POST
    | `PUT
    | `DELETE
    | `CONNECT
    | `OPTIONS
    | `TRACE
    | `OTHER of string ]

  val meth : t -> meth
  val request_target : t -> string
  val http_version : t -> int * int
  val headers : t -> header list
  val client_addr : t -> Lwt_unix.sockaddr
  val accept_encodings : t -> (encoding list, error) result

  (** {2 Pretty Printers} *)

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

val respond_with_bigstring :
     conn:Lwt_unix.file_descr
  -> status_code:int
  -> reason_phrase:string
  -> content_type:string
  -> bigstring
  -> unit Lwt.t

(** [request_handler] represents a request handler. *)
type request_handler = conn:Lwt_unix.file_descr -> Request.t -> unit Lwt.t

(** {2 [deflate] content encoding, decoding *)

val deflate_decode : bigstring -> (string, error) result
val deflate_encode : bigstring -> string

(** {2 [gzip] content encoding, decoding *)

val gzip_decode : bigstring -> (string, error) result
val gzip_encode : ?level:int -> bigstring -> string

val supported_encodings : encoding list
(** [supported_encodings] returns a list of encoding support by [Summer]
    HTTP/1.1 web server *)

val read_body_chunks :
     conn:Lwt_unix.file_descr
  -> Request.t
  -> on_chunk:(chunk:bigstring -> len:int -> chunk_extension list -> unit Lwt.t)
  -> (Request.t, error) Lwt_result.t
(** [read_body_chunks] supports reading request body when
    [Transfer-Encoding: chunked] is present in the request headers. *)

val read_body_content :
  conn:Lwt_unix.file_descr -> Request.t -> (bigstring, error) Lwt_result.t
(** [read_body_content] reads and returns request body content as bigstring. *)

(** {2 HTTP server} *)

val start : port:int -> request_handler -> 'a
(** [start port request_handler] Starts HTTP/1.1 server at [port]. *)

(** {2 Pretty printers} *)

val pp_encoder_name : Format.formatter -> encoder_name -> unit
val pp_encoding : Format.formatter -> encoding -> unit
