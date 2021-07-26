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
  let rec read_content content_length reader context body =
    Summer.read_content ~read_buf_size:10 content_length reader context
    >>= function
    | `Content buf ->
        read_content content_length reader context (Cstruct.append body buf)
    | `End -> Lwt.return (Ok body)
    | `Error e -> Lwt.return (Error e)
  in
  Summer.start ~port:!port (fun context ->
      let req = Summer.request context in
      let reader = Summer.body_reader context in
      Lwt_result.(
        lift (Summer.content_length req)
        >>= fun len -> read_content len reader context Cstruct.empty)
      >>= function
      | Ok body ->
          let text =
            Format.sprintf "%s\n\n%s" (Summer.show_request req)
              (Cstruct.to_string body)
            |> Lwt_bytes.of_string
          in
          Summer.respond_with_bigstring ~status_code:200 ~reason_phrase:"OK"
            ~content_type:"text/plain" text context
      | Error _ -> Lwt.return () )
