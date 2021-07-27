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
  let rec read_content context body state =
    state
    >>= function
    | Summer.Partial {body= buf; continue} ->
        read_content context (Cstruct.append body buf) (continue ())
    | Done -> Lwt.return (Ok body)
  in
  Summer.start ~port:!port (fun context req ->
      let content_length = Summer.content_length req in
      Summer.request_body ~read_buffer_size:10 ~content_length context
      |> read_content context Cstruct.empty
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
