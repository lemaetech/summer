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
val client_addr : request -> Lwt_unix.sockaddr
val content_length : request -> int
val pp_request : Format.formatter -> request -> unit
val show_request : request -> string

val read_body : request -> t -> Cstruct.t Lwt.t
(** [read_body request t] returns request body. *)

(** {1 Response} *)

(** See {{:https://tools.ietf.org/html/rfc7231#section-6.2} RFC7231§6.2} for
    more details. *)
type informational = [`Continue | `Switching_protocols]

(** See {{:https://tools.ietf.org/html/rfc7231#section-6.3} RFC7231§6.3} for
    more details. *)
type successful =
  [ `OK
  | `Created
  | `Accepted
  | `Non_authoritative_information
  | `No_content
  | `Reset_content
  | `Partial_content ]

(** See {{:https://tools.ietf.org/html/rfc7231#section-6.4} RFC7231§6.4} for
    more details. *)
type redirection =
  [ `Multiple_choices
  | `Moved_permanently
  | `Found
  | `See_other
  | `Not_modified
  | `Use_proxy
  | `Temporary_redirect ]

(** See {{:https://tools.ietf.org/html/rfc7231#section-6.5} RFC7231§6.5} for
    more details. *)
type client_error =
  [ `Bad_request
  | `Unauthorized
  | `Payment_required
  | `Forbidden
  | `Not_found
  | `Method_not_allowed
  | `Not_acceptable
  | `Proxy_authentication_required
  | `Request_timeout
  | `Conflict
  | `Gone
  | `Length_required
  | `Precondition_failed
  | `Payload_too_large
  | `Uri_too_long
  | `Unsupported_media_type
  | `Range_not_satisfiable
  | `Expectation_failed
  | `Upgrade_required
  | `I_m_a_teapot
  | `Enhance_your_calm ]

(** See {{:https://tools.ietf.org/html/rfc7231#section-6.6} RFC7231§6.6} for
    more details. *)
type server_error =
  [ `Internal_server_error
  | `Not_implemented
  | `Bad_gateway
  | `Service_unavailable
  | `Gateway_timeout
  | `Http_version_not_supported ]

(** The status codes defined in the HTTP 1.1 RFCs *)
type standard_status =
  [informational | successful | redirection | client_error | server_error]

(** See {{:https://tools.ietf.org/html/rfc7231#section-6} RFC7231§6} for more
    details on http response codes *)
type status = [standard_status | `Status of int]

val reason_phrase : standard_status -> string
(** [reason_phrase standard] is the example reason phrase provided by RFC7231
    for the {!type:standard_status} status code. The RFC allows servers to use
    reason phrases besides these in responses. *)

val to_code : status -> int
(** [to_code t] is the integer representation of [t]. *)

val of_code : int -> status
(** [of_code code] returns {!status} represented by [code]. It raises exception
    if [code] is not a valid HTTP code. *)

val respond_with_bigstring :
     t
  -> status_code:int
  -> reason_phrase:string
  -> content_type:string
  -> Cstruct.buffer
  -> unit Lwt.t

(** {2 Handler} *)

(** ['a handler] represents a connection handler. *)
type 'a handler = t -> request -> 'a Lwt.t

val io_buffer_size : int

val start : port:int -> unit handler -> unit
(** [start port request_handler] Starts HTTP/1.1 server at [port]. *)
