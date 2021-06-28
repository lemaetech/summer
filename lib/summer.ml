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
    <|> digit
    <|> alpha

  let token = take ~at_least:1 tchar >>= string_of_chars

  (*-- https://datatracker.ietf.org/doc/html/rfc3986#section-2.3 --*)
  let unreserved =
    alpha
    <|> digit
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
    let ipv4_address =
      let dec_octet =
        let alt1 = digit >>= fun c -> string_of_chars [c] in
        let alt2 =
          (* %x31-39 DIGIT         ; 10-99 *)
          char_if (function '\x32' .. '\x39' -> true | _ -> false)
          >>= fun c1 -> digit >>= fun c2 -> string_of_chars [c1; c2] in
        let alt3 =
          (* "1" 2DIGIT            ; 100-199 *)
          char '1'
          >>= fun c1 ->
          take ~at_least:2 ~up_to:2 digit >>= fun l -> string_of_chars (c1 :: l)
        in
        let alt4 =
          (* "2" %x30-34 DIGIT     ; 200-249 *)
          ( char '2'
          , char_if (function '\x30' .. '\x34' -> true | _ -> false)
          , digit )
          <$$$> fun c1 c2 c3 -> Format.sprintf "%c%c%c" c1 c2 c3 in
        let alt5 =
          (* "25" %x30-35          ; 250-255 *)
          ( string_cs "25"
          , char_if (function '\x30' .. '\x35' -> true | _ -> false) )
          <$$> fun s1 c1 -> Format.sprintf "%s%c" s1 c1 in
        any [alt1; alt2; alt3; alt4; alt5] in
      let* o1 = dec_octet <* char '.' in
      let* o2 = dec_octet <* char '.' in
      let* o3 = dec_octet <* char '.' in
      let+ o4 = dec_octet in
      Format.sprintf "%s.%s.%s.%s" o1 o2 o3 o4 in
    let ipv6address =
      let h16 = take ~at_least:1 ~up_to:4 hex_digit >>= string_of_chars in
      let ls32 =
        let alt1 = (h16, char ':', h16) <$$$> fun s1 _ s2 -> [s1; s2] in
        let alt2 = ipv4_address >>| fun s -> [s] in
        alt1 <|> alt2 in
      let join a b = a @ b in
      let col_col = string_cs "::" in
      let h16_exactly n =
        (take ~at_least:n ~up_to:n (h16 <* char ':'), ls32) <$$> join in
      let h16_upto n =
        optional
          ( take ~up_to:n (h16 <* char ':')
          >>= fun a1 -> h16 >>| fun a2 -> a1 @ [a2] )
        >>| (function Some l -> l | None -> [])
        <* col_col in
      let alt1 = (*-- 6( h16 ":" ) ls32 --*) (h16_exactly 6, ls32) <$$> join in
      let alt2 =
        (*-- "::" 5( h16 ":" ) ls32 --*)
        col_col *> ((h16_exactly 5, ls32) <$$> join) in
      let alt3 =
        (*-- [               h16 ] "::" 4( h16 ":" ) ls32 --*)
        let* a1 = optional h16 >>| function Some x -> [x] | None -> [] in
        let+ a2 = col_col *> ((h16_exactly 4, ls32) <$$> join) in
        a1 @ a2 in
      let alt4 =
        (*-- [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32 --*)
        (h16_upto 1, h16_exactly 3) <$$> join in
      let alt5 =
        (*-- [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32 --*)
        (h16_upto 2, h16_exactly 2) <$$> join in
      let alt6 =
        (*-- [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32 --*)
        (h16_upto 3, h16_exactly 1) <$$> join in
      let alt7 =
        (*-- [ *4( h16 ":" ) h16 ] "::"              ls32 --*)
        (h16_upto 4, ls32) <$$> join in
      let alt8 =
        (*-- [ *5( h16 ":" ) h16 ] "::"              h16 --*)
        (h16_upto 5, h16) <$$> fun a1 a2 -> a1 @ [a2] in
      let alt9 =
        (*-- [ *6( h16 ":" ) h16 ] "::" --*)
        h16_upto 6 in
      any [alt1; alt2; alt3; alt4; alt5; alt6; alt7; alt8; alt9]
      >>| fun ip6addrs -> `Ipv6address ip6addrs in
    let ipvfuture =
      let* hex_digits =
        char 'v' *> take ~at_least:1 hex_digit <* char '.' >>= string_of_chars
      in
      let+ txt =
        take ~at_least:1 (unreserved <|> sub_delims <|> colon) >>| to_string
      in
      `Ipvfuture (hex_digits, txt) in
    let ip_literal = char '[' *> (ipv6address <|> ipvfuture) <* char ']' in
    let reg_name =
      take (unreserved <|> pct_encoded <|> sub_delims)
      >>| fun reg_name' -> `Regname (to_string reg_name') in
    any [ip_literal; (ipv4_address >>| fun addr -> `Ipv4address addr); reg_name]
    >>| fun host' -> `Host host'

  let port = take digit >>= string_of_chars

  let authority =
    let* userinfo' =
      optional (userinfo <* char '@')
      >>| function Some info -> Some (`Userinfo info) | None -> None
    in
    let* host' = host in
    let+ port' = optional (char ':' *> port >>| fun p -> `Port p) in
    `Authority (userinfo', host', port')

  let scheme =
    let* c1 = alpha in
    let* l = take (alpha <|> digit <|> char '+' <|> char '-' <|> char '.') in
    string_of_chars (c1 :: l)

  let segment = take pchar >>| to_string

  let path_abempty =
    take (char '/' *> segment) >>| fun paths -> `Path_abempty paths

  (*-- https://datatracker.ietf.org/doc/html/rfc3986#section-3
     hier-part   = "//" authority path-abempty
                  / path-absolute
                  / path-rootless
                  / path-empty
    --*)
  let hier_part =
    let alt1 =
      string_cs "//"
      *> ( (authority, path_abempty)
         <$$> fun authority' path_abempty' -> (authority', path_abempty') )
    in
    any [alt1] >>| fun part' -> `Hier_part part'

  (*-- https://datatracker.ietf.org/doc/html/rfc3986#section-3.4 --*)
  let query =
    optional
      ( char '?'
        *> take
             (any
                [ pchar
                ; (char '/' >>| fun c -> `Char c)
                ; (char '?' >>| fun c -> `Char c) ] )
      >>| fun query' -> `Query (to_string query') )

  (*-- https://datatracker.ietf.org/doc/html/rfc3986#section-4.3 --*)
  let absolute_uri =
    let* scheme' = scheme >>| fun s -> `Scheme s in
    let* hier_part' = char ':' *> hier_part in
    let+ query' = query in
    `Absolute_uri (scheme', hier_part', query')

  (*-- request-target = origin-form / absolute-form / authority-form /
    asterisk-form --*)
  let request_target =
    let origin_form =
      let* abs_path = take ~at_least:1 (char '/' *> segment) in
      let+ query' = query in
      `Origin (abs_path, query') in
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
