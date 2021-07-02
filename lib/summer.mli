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

(** Request body content length type. *)
type content_length = int

(** Request header - (name * value) *)
type header = string * string

(** Header recevied as part of Transfer-Encoding:chunked body *)
type trailer_header = header

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

  val meth : t -> meth
  val request_target : t -> string
  val http_version : t -> int * int
  val headers : t -> header list
  val body_type : t -> [`Content | `Chunked | `None]
  val client_addr : t -> Lwt_unix.sockaddr
  val pp : Format.formatter -> t -> unit
  val show : t -> string
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
  -> (trailer_header list * content_length, string) Lwt_result.t
(** [read_body_chunks] supports reading request body when
    [Transfer-Encoding: chunked] is present in the request headers. *)

val read_body_content :
  conn:Lwt_unix.file_descr -> Request.t -> (bigstring, string) Lwt_result.t
(** [read_body_content] reads and returns request body content as bigstring. *)

(** {2 HTTP server} *)

val start : port:int -> request_handler -> 'a
(** [start port request_handler] Starts HTTP/1.1 server at [port]. *)
