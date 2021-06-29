(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

(* Summer is a http 1.1 server as outlined in the following RFCs
   1. https://datatracker.ietf.org/doc/html/rfc7230
   2. https://datatracker.ietf.org/doc/html/rfc7231 *)
open Lwt.Infix

(* Parsers defined at https://datatracker.ietf.org/doc/html/rfc7230#appendix-B *)
module Parser (P : Reparse.PARSER) = struct
  open P

  let pair a b = (a, b)

  (*-- https://datatracker.ietf.org/doc/html/rfc7230#appendix-B --*)
  let tchar =
    char_if (function
      | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
       |'`' | '|' | '~' ->
          true
      | _ -> false )
    <|> digit
    <|> alpha

  let token = take ~at_least:1 tchar >>= string_of_chars

  (*-- request-line = method SP request-target SP HTTP-version CRLF -- *)
  let request_line =
    let* meth = token <* space in
    let* request_target =
      take_while ~while_:(is_not space) any_char <* space >>= string_of_chars
    in
    let+ http_version =
      (*-- https://datatracker.ietf.org/doc/html/rfc7230#section-2.6 --*)
      (string_cs "HTTP/" *> digit <* char '.', digit) <$$> pair <* crlf
    in
    (meth, request_target, http_version)

  let ows = skip (space <|> htab) *> unit

  (*-- https://datatracker.ietf.org/doc/html/rfc7230#section-3.2 --*)
  let header_fields =
    let field_name = token in
    let field_value =
      let field_content =
        let c2 =
          optional
            (let* c1 = skip ~at_least:1 (space <|> htab) *> vchar in
             string_of_chars [' '; c1])
          >>| function Some s -> s | None -> "" in
        (vchar, c2) <$$> fun c1 c2 -> Format.sprintf "%c%s" c1 c2 in
      take field_content >>| String.concat "" in
    let header_field =
      (field_name <* char ':' <* ows, field_value <* ows) <$$> pair <* crlf
    in
    take header_field
end

module Request = struct
  type t = {meth: meth}

  (* https://datatracker.ietf.org/doc/html/rfc7231#section-4 *)
  and meth =
    [`GET | `HEAD | `POST | `PUT | `DELETE | `CONNECT | `OPTIONS | `TRACE]

  (* let request_line = *)
  (*   let meth = token <* space in *)
  (*   () *)
end

let start port handler =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt_engine.set (new Lwt_engine.libev ()) ;
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket ~backlog:11_000 listen_address
        handler
      >>= fun _server -> Lwt.return () ) ;
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
