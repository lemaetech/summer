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

let about_handler _req = Lwt.return (Summer.text "about page")

let echo_handler req =
  let+ body =
    Lwt.catch (fun () -> Summer.body req) (fun _exn -> Lwt.return "")
  in
  let body = Format.asprintf "%a\n\n%s" Summer.pp_request req body in
  Summer.text body

let router =
  Wtr.create
    [ {%wtr| get     ; /about    |} about_handler
    ; {%wtr| get,post; /echo     |} echo_handler ]

let app = Summer.router router @@ Summer.not_found

let () =
  let port = ref 3000 in

  Arg.parse
    [("-p", Arg.Set_int port, " Listening port number (3000 by default)")]
    ignore "An echo HTTP server using summer!" ;
  Summer.start ~port:!port app
