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

(** [t] represents a HTTP/1.1 sever *)
type t

(** [request] represents a HTTP/1.1 request *)
and request

(** [meth] represents request methods. *)
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

(** [header] represents a HTTP header, a tuple of (name * value) *)
and header = string * string

(** ['a handler] represents a connection handler. *)
and 'a handler = t -> request -> 'a Lwt.t

val io_buffer_size : int

(** {2 Request} *)

val meth : request -> meth
val target : request -> string
val http_version : request -> int * int
val headers : request -> header list
val client_addr : request -> Lwt_unix.sockaddr
val content_length : request -> int
val pp_request : Format.formatter -> request -> unit
val show_request : request -> string

val read_body : request -> t -> Cstruct.t Lwt.t
(** [read_body request t] returns request body. *)

(** {2 Response} *)

val respond_with_bigstring :
     t
  -> status_code:int
  -> reason_phrase:string
  -> content_type:string
  -> Cstruct.buffer
  -> unit Lwt.t

(** {2 HTTP server} *)

val start : port:int -> unit handler -> unit
(** [start port request_handler] Starts HTTP/1.1 server at [port]. *)
