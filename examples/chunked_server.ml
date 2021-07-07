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
  Summer.start ~port:!port (fun context ->
      let buf = ref (Cstruct.create 0) in
      Summer.read_body_chunks
        ~on_chunk:(fun ~chunk ~len _exts ->
          buf := Cstruct.append !buf (Cstruct.of_bigarray chunk ~len) ;
          Lwt.return () )
        context
      >>= fun () ->
      let content = Cstruct.to_bigarray !buf in
      Summer.respond_with_bigstring ~status_code:200 ~reason_phrase:"OK"
        ~content_type:"text/plain" content context )
