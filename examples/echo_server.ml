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
  Summer.start ~port:!port (fun t req ->
      Summer.read_body req t
      >>= function
      | body ->
          let text =
            Format.sprintf "%s\n\n%s" (Summer.show_request req)
              (Cstruct.to_string body)
            |> Lwt_bytes.of_string
          in
          Summer.respond_with_bigstring t ~status_code:200 ~reason_phrase:"OK"
            ~content_type:"text/plain" text )
