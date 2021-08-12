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

(** {1 Types} *)

(** [request] represents a HTTP/1.1 request *)
type request

(** [method'] represents request methods. *)
and method' =
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

and response

(** See {{:https://tools.ietf.org/html/rfc7231#section-6} RFC7231§6} for more
    details on http response codes *)
and response_code

(** ['a handler] represents a connection handler. *)
and handler = request -> response Lwt.t

and middleware = handler -> handler

and memory_session

(** A value used for encryption/decryption. *)
and key

(** Represents an encrypted content. *)
and secret

(** {1 Request} *)

val method_equal : method' -> method' -> bool
val method' : request -> method'
val target : request -> string
val http_version : request -> int * int
val headers : request -> header list
val client_addr : request -> string
val content_length : request -> int option

val cookies : request -> Http_cookie.t list
(** [cookies request] returns a list of cookies in [request]. See
    {:https://tools.ietf.org/html/rfc6265#section-4.2} *)

val find_cookie : string -> request -> Http_cookie.t option
(** [find_cookie cookie_name request] is [Some cookie] if
    [Http_cookie.name = cookie_name], otherwise [None]. *)

val request_header : string -> request -> string option
(** [request_header header request] returns request header value associated with
    [header]. *)

val body : request -> string Lwt.t
(** [body request t] returns request body. *)

(** {1 Pretty printers} *)

val pp_request : Format.formatter -> request -> unit
val pp_method : Format.formatter -> method' -> unit

(** {1 Form} *)

val form_multipart :
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

val form_multipart_all :
  request -> (Http_multipart_formdata.part_header * Cstruct.t) list Lwt.t
(** [multipart_all request] is a non streaming version of {!val:multipart}. It
    returns a list of multipart tuples - (part_header, body) - where each tuple
    represents a multipart part. *)

val form_urlencoded : request -> (string * string list) list Lwt.t
(** Returns a list of [name, value list] pairs from form data encoded in
    [application/x-www-form-urlencoded] format. See
    {{:https://tools.ietf.org/html/rfc1866#section-8.2.1} RFC 1866 §8.2.1}. *)

(** {1 Response} *)

val ok : response_code
(** HTTP 200 response code *)

val internal_server_error : response_code
(** HTTP 500 (Internal Server Error) response code *)

val bad_request : response_code
(** HTTP 400 Bad Request response code *)

val response_code : ?reason_phrase:string -> int -> response_code
(** [response code] returns {!type:response_code} represented by [code]. It
    raises exception if [code] is not a valid HTTP code.

    [reason_pharse] default value is "unknown". This is the reason phrase used
    if [code] is not a standard http response code.

    See {{:https://tools.ietf.org/html/rfc7231#section-6} RFC7231§6} for valid
    response codes. *)

val code_int : response_code -> int
(** [response_code_int response_code] returns an integer representation of
    [response_code]. *)

val code_reason_phrase : response_code -> string
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

val tyxml : Tyxml.Html.doc -> response

(** {2 Cookies} *)

val add_cookie : Http_cookie.t -> response -> response
(** [add_cookie cookie response] adds [cookie] to [response]. If a [cookie] with
    the same cookie nme already exists in response, it is replaced by the given
    [cookie]. *)

val remove_cookie : string -> response -> response
(** [remove_cookie cookie_name response] removes cookie with [cookie_name] from
    response if it exists. *)

val not_found : handler

(** {2 Headers} *)

val add_header : name:string -> string -> response -> response
val remove_header : string -> response -> response

(** {1 Encryption/Decryption} *)

val key : int -> key
(** [key sz] is {!type:key} which has [sz] count of bytes. *)

val key_to_base64 : key -> string
val key_of_base64 : string -> (key, string) result
val encrypt : key -> string -> secret
val decrypt_base64 : key -> string -> (string, string) result

(** {1 Session} *)

val session_put : key:string -> string -> request -> unit Lwt.t
(** [session_put ~key value req] stores session [value] corresponding to [key]. *)

val session_find : string -> request -> string option
(** [session_find key req] returns a session value corresponding to [key]. *)

val session_all : request -> (string * string) list
(** [session_all req] returns a list of session objects (key,value). *)

val cookie_session : key -> middleware

val memory_session :
     ?expires:Http_cookie.date_time
  -> ?max_age:int
  -> ?cookie_name:string
  -> unit
  -> memory_session
(** [memory_session] creates a new mession_session for the application.

    If neither [expires] or [max_age] is given default session expires when the
    user closes the browser session. *)

val in_memory : memory_session -> middleware
(** [in_memory memory_session] is a middleware to handle sessions in memory.
    In-memory sessions are not persisted in between application restarts. *)

(** {1 Routing} *)

val router : handler Wtr.t -> middleware

val start : port:int -> handler -> unit
(** [start port request_handler] Starts HTTP/1.1 server at [port]. *)
