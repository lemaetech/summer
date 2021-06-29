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
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Context : sig
  type t

  val request : t -> Request.t
  val connection : t -> Lwt_unix.file_descr
end

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val respond_with_bigstring :
     Context.t
  -> status_code:int
  -> reason_phrase:string
  -> content_type:string
  -> bigstring
  -> unit Lwt.t

type request_handler = Context.t -> unit Lwt.t

val start : port:int -> request_handler -> 'a
