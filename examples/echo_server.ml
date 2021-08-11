(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

open Lwt.Syntax
open Tyxml.Html

let about _req =
  html
    (head (title (txt "Summer: Echo App")) [])
    (body [div [txt "About page"]])
  |> Summer.tyxml |> Lwt.return

let echo req =
  let+ body =
    Lwt.catch (fun () -> Summer.body req) (fun _exn -> Lwt.return "")
  in
  Format.asprintf "%a@.@.%s" Summer.pp_request req body |> Summer.text

let say_hello name _req =
  html
    (head (title (txt "Summer: Echo App")) [])
    (body [div [txt ("Hello " ^ name ^ "!")]])
  |> Summer.tyxml |> Lwt.return

let counter : Summer.handler =
 fun req ->
  let key = "counter" in
  let counter =
    match Summer.session_find key req with
    | Some v -> 1 + int_of_string v
    | None -> 0
  in
  let* () = Summer.session_put ~key (string_of_int counter) req in
  html
    (head (title (txt "Summer: Echo App")) [])
    (body [div [txt ("Hello " ^ string_of_int counter ^ "!")]])
  |> Summer.tyxml |> Lwt.return

let router =
  Wtr.create
    [ {%wtr| get     ; /about               |} about
    ; {%wtr| get,post; /echo                |} echo
    ; {%wtr| get     ; /say_hello/:string   |} say_hello
    ; {%wtr| get     ; /counter             |} counter ]

let app =
  let session = Summer.memory_session () in
  Summer.in_memory session @@ Summer.router router @@ Summer.not_found

let () =
  let port = ref 3000 in
  Arg.parse
    [("-p", Arg.Set_int port, " Listening port number (3000 by default)")]
    ignore "An echo HTTP server using summer!" ;
  Summer.start ~port:!port app
