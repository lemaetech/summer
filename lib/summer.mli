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
  | `Method of string ]

(** Represents a request body reader *)
and body_reader

and content_length = int

(** [header] represents a HTTP header, a tuple of (name * value) *)
and header = string * string

(** [error] represents an error string *)
and error = string

(** ['a handler] represents a connection handler. *)
and 'a handler = context -> 'a Lwt.t

(** [context] holds data for [handler] function. *)
and context

(** {2 Request} *)

val request : context -> request
val meth : request -> meth
val target : request -> string
val http_version : request -> int * int
val headers : request -> header list
val client_addr : request -> Lwt_unix.sockaddr
val content_length : request -> (content_length, error) result
val pp_request : Format.formatter -> request -> unit
val show_request : request -> string

(** {2 Context} *)

val conn : context -> Lwt_unix.file_descr

val body_reader : context -> body_reader
(** [body_reader context] returns a body_reader. *)

val read_content :
     content_length
  -> ?read_buf_size:int
  -> body_reader
  -> context
  -> [`Content of Cstruct.t | `End | `Error of error] Lwt.t

(** {2 Response} *)

val respond_with_bigstring :
     status_code:int
  -> reason_phrase:string
  -> content_type:string
  -> Cstruct.buffer
  -> unit handler

(** {2 HTTP server} *)

val start : port:int -> unit handler -> unit
(** [start port request_handler] Starts HTTP/1.1 server at [port]. *)
