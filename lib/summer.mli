(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

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
  val headers : t -> (string * string) list
  val client_addr : t -> Lwt_unix.sockaddr
  val connection_fd : t -> Lwt_unix.file_descr
  val pp : Format.formatter -> t -> unit
end

module Response : sig
  type bigstring =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t

  val t : Request.t -> t

  val respond_with_bigstring :
       t
    -> status_code:int
    -> reason_phrase:string
    -> content_type:string
    -> bigstring
    -> unit Lwt.t
end

type request_handler = Request.t -> unit Lwt.t

val start : int -> request_handler -> 'a
