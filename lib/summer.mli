(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

(** [Summer] is a HTTP/1.1 server.

    It aims to implement the following HTTP rfcs:

    - https://datatracker.ietf.org/doc/html/rfc7230
    - https://datatracker.ietf.org/doc/html/rfc7231 *)

(** {1 Request} *)

(** [request] represents a HTTP/1.1 request *)
type request

(** [meth] represents request methods. *)
type meth =
  [ `GET
  | `HEAD
  | `POST
  | `PUT
  | `DELETE
  | `CONNECT
  | `OPTIONS
  | `TRACE
  | `Method of string ]

(** [header] represents a HTTP header, a tuple of (name * value) *)
type header = string * string

val meth : request -> meth
val target : request -> string
val http_version : request -> int * int
val headers : request -> header list
val client_addr : request -> string
val content_length : request -> int
val pp_request : Format.formatter -> request -> unit
val show_request : request -> string

val read_body : request -> string Lwt.t
(** [read_body request t] returns request body. *)

(** {1 Response} *)

type response

(** See {{:https://tools.ietf.org/html/rfc7231#section-6} RFC7231§6} for more
    details on http response codes *)
type response_code

val response_code_200 : response_code
(** HTTP 200 response code *)

val response_code_500 : response_code
(** HTTP 500 (Internal Server Error) response code *)

val response_code_400 : response_code
(** HTTP 400 Bad Request response code *)

val response_code : ?reason_phrase:string -> int -> response_code
(** [response code] returns {!response_code} represented by [code]. It raises
    exception if [code] is not a valid HTTP code.

    [reason_pharse] default value is "unknown". This is the reason phrase used
    if [code] is not a standard http response code.

    See {{:https://tools.ietf.org/html/rfc7231#section-6} RFC7231§6} for valid
    response codes. *)

val response_code_int : response_code -> int
(** [response_code_int response_code] returns an integer representation of
    [response_code]. *)

val response_code_reason_phrase : response_code -> string
(** [response_code_reason_phrase response_code] returns reason phrase for
    [response_code]. *)

val response :
  ?response_code:response_code -> ?headers:header list -> string -> response
(** [response ~response_code ~headers body] returns a {!type:response}. The
    default value for [response_code] is [200]. *)

val response_bigstring :
     ?response_code:response_code
  -> ?headers:header list
  -> Cstruct.buffer
  -> response
(** [response_bigstring] similar to {!response} except body is a bigstring. *)

(** {1 Handler} *)

(** ['a handler] represents a connection handler. *)
type handler = request -> response Lwt.t

val io_buffer_size : int

val start : port:int -> handler -> unit
(** [start port request_handler] Starts HTTP/1.1 server at [port]. *)
