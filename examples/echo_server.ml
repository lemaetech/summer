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

let () =
  let port = ref 3000 in
  Arg.parse
    [("-p", Arg.Set_int port, " Listening port number (3000 by default)")]
    ignore "An echo HTTP server using summer!" ;
  Summer.start ~port:!port (fun t req ->
      let+ body = Summer.read_body req t in
      let text =
        Format.sprintf "%s\n\n%s" (Summer.show_request req)
          (Cstruct.to_string body)
      in
      Summer.response
        ~headers:[("content-type", "text/plain; charset=utf-8")]
        text )
