(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)
open Lwt.Infix

let () =
  let port = ref 3000 in
  Arg.parse
    [("-p", Arg.Set_int port, " Listening port number (3000 by default)")]
    ignore "An echo HTTP server using summer!" ;
  Summer.start ~port:!port (fun ~conn req ->
      ( match Summer.Request.body_type req with
      | `Content -> Summer.read_body_content ~conn req
      | `Chunked ->
          let buf = ref (Cstruct.create 0) in
          Lwt_result.(
            Summer.read_body_chunks req ~conn
              ~on_chunk:(fun ~chunk ~len _exts ->
                buf := Cstruct.append !buf (Cstruct.of_bigarray ~len chunk) ;
                Lwt.return () )
            >|= fun (_req : Summer.Request.t) -> Cstruct.to_bigarray !buf)
      | `None -> Lwt_result.return (Lwt_bytes.create 0) )
      >>= function
      | Ok buf ->
          Summer.respond_with_bigstring ~conn ~status_code:200
            ~reason_phrase:"OK" ~content_type:"text/plain" buf
      | Error e ->
          Summer.respond_with_bigstring ~conn ~status_code:500
            ~reason_phrase:"Internal Server Error" ~content_type:"text/plain"
            (Lwt_bytes.of_string e) )
