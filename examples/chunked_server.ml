(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

let () =
  let port = ref 3000 in
  Arg.parse
    [("-p", Arg.Set_int port, " Listening port number (3000 by default)")]
    ignore "An echo HTTP server using summer!" ;
  Summer.start ~port:!port (fun ~conn req ->
      match Summer.Request.body_type req with
      | `Content _content_length -> Lwt.return ()
      | `Chunked -> (
          let buf = ref (Cstruct.create 0) in
          Lwt.(
            Summer.read_body_chunks ~conn ~on_chunk:(fun ~chunk ~len _exts ->
                buf := Cstruct.append !buf (Cstruct.of_bigarray ~len chunk) ;
                Lwt.return () )
            >>= function
            | Ok _ ->
                Summer.respond_with_bigstring ~conn ~status_code:200
                  ~reason_phrase:"OK" ~content_type:"text/plain"
                  (Cstruct.to_bigarray !buf)
            | Error e ->
                Summer.respond_with_bigstring ~conn ~status_code:500
                  ~reason_phrase:"Internal Server Error"
                  ~content_type:"text/plain" (Lwt_bytes.of_string e)) )
      | `None -> Lwt.return () )
