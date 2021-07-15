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
  let rec read_body content_length reader context bufs =
    Summer.read_content ~read_buf_size:10 content_length reader context
    >>= function
    | `Content buf -> read_body content_length reader context (buf :: bufs)
    | `End -> Lwt.return (Ok (List.rev bufs |> Cstruct.concat))
    | `Error e -> Lwt.return (Error e)
  in
  Summer.start ~port:!port (fun context ->
      let req = Summer.request context in
      ( match Summer.body_type req with
      | Ok (`Content len) ->
          let reader = Summer.body_reader context in
          read_body len reader context []
      | Ok `None -> Lwt.return (Ok Cstruct.empty)
      | Ok _ -> failwith ""
      | Error _ -> Lwt.return (Error "") )
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
