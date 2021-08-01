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
val content_length : request -> int option

val request_header : string -> request -> string option
(** [request_header header request] returns request header value associated with
    [header]. *)

val body : request -> string Lwt.t
(** [body request t] returns request body. *)

(** {2 Pretty printers} *)

val pp_request : Format.formatter -> request -> unit
val pp_meth : Format.formatter -> meth -> unit

(** {2 Form} *)

val multipart :
     ?body_buffer_size:int
  -> request
  -> [ `End  (** Reading of multipart form is complete. *)
     | `Header of Http_multipart_formdata.part_header
       (** Multipart part header. *)
     | `Body of Cstruct.t
       (** Multipart part body data. The size of body is determined by
           [body_buffer_size]. *)
     | `Body_end  (** End of multipart body. *) ]
     Lwt.t
(** [multipart ?body_buffer_size request] streams HTTP [multipart/formdata]
    content-type data. [multipart/formdata] content-type is used when uploading
    files to HTTP web servers.

    [body_buffer_size] denotes the size of bytes to read per [multipart] [`Body]
    part. The default value is {!io_buffer_size}.

    If the request is an invalid [multipart/formdata] content-type then it
    returns [400 Bad request] response. *)

val multipart_all :
  request -> (Http_multipart_formdata.part_header * Cstruct.t) list Lwt.t
(** [multipart_all request] is a non streaming version of {!val:multipart}. It
    returns a list of multipart tuples - (part_header, body) - where each tuple
    represents a multipart part. *)

val urlencoded_form : request -> (string * string list) list Lwt.t
(** Returns a list of [name, value list] pairs from form data encoded in
    [application/x-www-form-urlencoded] format. See
    {{:https://tools.ietf.org/html/rfc1866#section-8.2.1} RFC 1866 §8.2.1}. *)

(** {2 Cookies} *)

val cookies : request -> Http_cookie.t list
(** [cookies request] returns a list of cookies in [request]. See
    {:https://tools.ietf.org/html/rfc6265#section-4.2} *)

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
(** [response code] returns {!type:response_code} represented by [code]. It
    raises exception if [code] is not a valid HTTP code.

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
(** [response_bigstring] similar to {!type:response} except body is a bigstring. *)

val text : string -> response
(** [text body] creates a response with HTTP status 200 and content-type of
    ["text/plain; charset=utf-8"]. *)

val html : string -> response
(** [text body] creates a response with HTTP status 200 and content-type of
    ["text/html; charset=UTF-8"]. *)

(** {2 Cookies} *)

val add_cookie : Http_cookie.t -> response -> response
(** [add_cookie cookie response] adds [cookie] to [response]. If a [cookie] with
    the same cookie nme already exists in response, it is replaced by the given
    [cookie]. *)

val remove_cookie : string -> response -> response
(** [remove_cookie cookie_name response] removes cookie with [cookie_name] from
    response if it exists. *)

(** {2 Headers} *)

val add_header : name:string -> string -> response -> response
val remove_header : string -> response -> response

(** {1 Handler} *)

(** ['a handler] represents a connection handler. *)
type handler = request -> response Lwt.t

val io_buffer_size : int

val start : port:int -> handler -> unit
(** [start port request_handler] Starts HTTP/1.1 server at [port]. *)
