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

(** [request] represents a HTTP/1.1 request *)
type request

and meth =
  [ `GET
  | `HEAD
  | `POST
  | `PUT
  | `DELETE
  | `CONNECT
  | `OPTIONS
  | `TRACE
  | `OTHER of string ]

(** Represents a request body reader *)
and body_reader

and body_type =
  [ `Chunked
  | `Content of content_length
  | `Multipart of content_length * boundary
  | `None ]

and content_length = int

and boundary = Http_multipart_formdata.boundary

(** Represents a chunk of data read by {!type:body_reader}. *)
and chunk_body =
  {data: bigstring; size: int; chunk_extensions: chunk_extension list}

(** [chunk_extension] is an optional component of a chunk. It is defined at
    https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.1 *)
and chunk_extension = {name: string; value: string option}

(** [header] represents a HTTP header, a tuple of (name * value) *)
and header = string * string

and bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** [error] represents an error string *)
and error = string

(** [accept_encoding] represents [Accept-Encoding] and [Content-Encoding] header
    values. https://datatracker.ietf.org/doc/html/rfc7231#section-5.3.4 *)
and encoding = {encoder: encoder; weight: float option}

and encoder =
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

(** ['a handler] represents a connection handler. *)
and 'a handler = context -> 'a Lwt.t

(** [context] holds data for [handler] function. *)
and context
(** {2 Request} *)

val meth : request -> meth

val target : request -> string

val http_version : request -> int * int

val headers : request -> header list

val client_addr : request -> Lwt_unix.sockaddr

val content_length : request -> (content_length, error) result

val accept_encoding : request -> (encoding list, error) result

val content_encoding : request -> encoder list

val add_header : header -> request -> unit

val remove_header : string -> request -> unit

val pp_request : Format.formatter -> request -> unit

val show_request : request -> string

(** {2 Handler} *)

val request : context -> request

val conn : context -> Lwt_unix.file_descr

(** {2 deflate content encoding, decoding} *)

val deflate_decode : bigstring -> (string, error) result

val deflate_encode : bigstring -> string

(** {2 gzip content encoding, decoding} *)

val gzip_decode : bigstring -> (string, error) result

val gzip_encode : ?level:int -> bigstring -> string

val supported_encodings : encoding list
(** [supported_encodings] returns a list of encoding supported by [Summer]
    HTTP/1.1 web server. The following encodings are supported: [gzip,deflate] *)

(** {2 Request Body Readers} *)

val body_type : context -> (body_type, error) result

val body_reader : context -> body_reader
(** [body_reader context] returns a body_reader. *)

val read_chunked :
     body_reader
  -> context
  -> [`Chunk of chunk_body | `End | `Error of error] Lwt.t
(** [read_body rdr] reads request body.

    If [Transfer-Encoding] header is present then each [`Body body] represents a
    request body 'chunk'. [body.chunk_extension] represents any chunk extensions
    present in the request chunk and [body.size] represents the chunk size.

    If [Content-Length] header is present then [`Body body] represents the
    content body. [body.chunk_extension] is [List.empty] and [body.size]
    represents the [Content-Length] value. *)

val read_content :
     content_length
  -> body_reader
  -> context
  -> [`Body of Cstruct.t | `End | `Error of error] Lwt.t

(** {2 Response} *)

val respond_with_bigstring :
     status_code:int
  -> reason_phrase:string
  -> content_type:string
  -> bigstring
  -> unit handler

(** {2 HTTP server} *)

val start : port:int -> unit handler -> unit
(** [start port request_handler] Starts HTTP/1.1 server at [port]. *)

(** {2 Pretty printers} *)

val pp_encoder : Format.formatter -> encoder -> unit

val pp_encoding : Format.formatter -> encoding -> unit
