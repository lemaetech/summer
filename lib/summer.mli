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

(** Request header - (name * value) *)
type header = string * string

(** [chunk_extension] is an optional component of a chunk. It is defined at
    https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.1 *)
type chunk_extension = {name: string; value: string option}

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

  (** Represents [Accept-Encodings] header value.
      https://datatracker.ietf.org/doc/html/rfc7231#section-5.3.4 *)
  type accept_encoding = {encoding: encoding; weight: float option}

  and encoding =
    [ `Compress
      (** Compress - https://datatracker.ietf.org/doc/html/rfc7230#section-4.2.1 *)
    | `Deflate
      (** Deflate - https://datatracker.ietf.org/doc/html/rfc7230#section-4.2.2 *)
    | `Gzip
      (** Gzip - https://datatracker.ietf.org/doc/html/rfc7230#section-4.2.3 *)
    | `Br  (** Br (Brotli) - https://datatracker.ietf.org/doc/html/rfc7932 *)
    | `Any
      (** Represented by '*': The asterisk "*" symbol in an Accept-Encoding
          field matches any available content-coding not explicitly listed in
          the header field. *)
    | `None  (** Represented by '' empty value.*)
    | `Other of string
      (** Any other encoding - possibly a custom one - not specified by the HTTP
          RFC 7230 or 7231 or 7932.*) ]

  val meth : t -> meth
  val request_target : t -> string
  val http_version : t -> int * int
  val headers : t -> header list
  val client_addr : t -> Lwt_unix.sockaddr
  val accept_encodings : t -> (accept_encoding list, string) result

  (** {2 Pretty Printers} *)

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val pp_coding : Format.formatter -> encoding -> unit
  val pp_accept_encoding : Format.formatter -> accept_encoding -> unit
end

type bigstring = Lwt_bytes.t

val respond_with_bigstring :
     conn:Lwt_unix.file_descr
  -> status_code:int
  -> reason_phrase:string
  -> content_type:string
  -> bigstring
  -> unit Lwt.t

(** {2 Request handling} *)

type request_handler = conn:Lwt_unix.file_descr -> Request.t -> unit Lwt.t

val read_body_chunks :
     conn:Lwt_unix.file_descr
  -> Request.t
  -> on_chunk:(chunk:bigstring -> len:int -> chunk_extension list -> unit Lwt.t)
  -> (Request.t, string) Lwt_result.t
(** [read_body_chunks] supports reading request body when
    [Transfer-Encoding: chunked] is present in the request headers. *)

val read_body_content :
  conn:Lwt_unix.file_descr -> Request.t -> (bigstring, string) Lwt_result.t
(** [read_body_content] reads and returns request body content as bigstring. *)

(** {2 HTTP server} *)

val start : port:int -> request_handler -> 'a
(** [start port request_handler] Starts HTTP/1.1 server at [port]. *)
