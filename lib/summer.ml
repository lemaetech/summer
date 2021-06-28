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

  let tchar =
    char_if (function
      | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
       |'`' | '|' | '~' ->
          true
      | _ -> false )
    <|> digit <|> alpha

  let token = take ~at_least:1 tchar >>= string_of_chars

  (*-- https://datatracker.ietf.org/doc/html/rfc3986#section-2.3 --*)
  let unreserved =
    alpha <|> digit
    <|> char_if (function '-' | '.' | '_' | '~' -> true | _ -> false)
    >>| fun c -> `Char c

  let pct_encoded =
    (char '%', hex_digit, hex_digit)
    <$$$> fun pct hex1 hex2 -> `String (Format.sprintf "%c%c%c" pct hex1 hex2)

  let sub_delims =
    char_if (function
      | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' -> true
      | _ -> false )
    >>| fun c -> `Char c

  let colon = char ':' >>| fun c -> `Char c
  let at = char '@' >>| fun c -> `Char c

  (*-- https://datatracker.ietf.org/doc/html/rfc3986#section-3.3 --*)
  let pchar = unreserved <|> pct_encoded <|> sub_delims <|> colon <|> at

  let to_string l =
    let buf = Buffer.create 0 in
    List.iter
      (function
        | `Char c -> Buffer.add_char buf c
        | `String s -> Buffer.add_string buf s )
      l ;
    Buffer.contents buf

  (*-- https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.1 --*)
  let userinfo =
    take (unreserved <|> pct_encoded <|> sub_delims <|> colon) >>| to_string

  (*-- https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.2 --*)
  let host =
    let ip_literal = return "" in
    let ipv4_address = return "" in
    let reg_name = return "" in
    ip_literal <|> ipv4_address <|> reg_name

  let segment = take pchar >>| to_string

  (*-- https://datatracker.ietf.org/doc/html/rfc3986#section-3.4 --*)
  let query =
    optional (char '?')
    >>= function
    | Some _ ->
        take
          (any
             [ pchar
             ; (char '/' >>| fun c -> `Char c)
             ; (char '?' >>| fun c -> `Char c) ] )
        >>| fun txt -> Some (to_string txt)
    | None -> return None

  let scheme =
    let* c1 = alpha in
    let* l = take (alpha <|> digit <|> char '+' <|> char '-' <|> char '.') in
    string_of_chars (c1 :: l)

  (*-- https://datatracker.ietf.org/doc/html/rfc3986#section-3
     hier-part   = "//" authority path-abempty
                  / path-absolute
                  / path-rootless
                  / path-empty
    --*)
  let hier_part =
    (* let userinfo = re *)
    (*   let authority = *)
    return ""

  (*-- https://datatracker.ietf.org/doc/html/rfc3986#section-4.3 --*)
  let absolute_uri =
    let* scheme' = scheme in
    let* hier_part' = char ':' *> hier_part in
    let+ query' = query in
    `Absolute_uri (scheme', hier_part', query')

  (*-- request-target = origin-form / absolute-form / authority-form /
      asterisk-form --*)
  let request_target =
    let absolute_path = take ~at_least:1 (char '/' *> segment) in
    let origin_form =
      absolute_path
      >>= fun abs_path ->
      optional (char '?')
      >>= (function Some _ -> query >>| Option.some | None -> return None)
      >>| fun query -> `Origin (abs_path, query) in
    let absolute_form = absolute_uri in
    origin_form <|> absolute_form
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
