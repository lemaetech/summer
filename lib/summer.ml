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
module Make_parser (P : Reparse.PARSER) = struct
  include P

  let pair a b = (a, b)

  (*-- https://datatracker.ietf.org/doc/html/rfc7230#appendix-B --*)
  let tchar =
    char_if (function
      | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
       |'`' | '|' | '~' ->
          true
      | _ -> false )
    <|> digit <|> alpha

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

module Parser = Make_parser (Reparse_lwt_unix.Fd)

module Request = struct
  type t =
    { meth: meth
    ; request_target: string
    ; http_version: int * int
    ; headers: (string * string) list
    ; client_addr: Lwt_unix.sockaddr
    ; connection_fd: Lwt_unix.file_descr }

  (* https://datatracker.ietf.org/doc/html/rfc7231#section-4 *)
  and meth =
    [ `GET
    | `HEAD
    | `POST
    | `PUT
    | `DELETE
    | `CONNECT
    | `OPTIONS
    | `TRACE
    | `OTHER of string ]

  let meth t = t.meth
  let request_target t = t.request_target
  let http_version t = t.http_version
  let headers t = t.headers
  let client_addr t = t.client_addr
  let connection_fd t = t.connection_fd

  let rec pp fmt t =
    let fields =
      [ Fmt.field "meth" (fun p -> p.meth) pp_meth
      ; Fmt.field "request_target" (fun p -> p.request_target) Fmt.string
      ; Fmt.(
          field "http_version"
            (fun p -> p.http_version)
            (pair ~sep:comma int int))
      ; Fmt.field "headers" (fun p -> p.headers) pp_headers ] in
    Fmt.record ~sep:Fmt.semi fields fmt t

  and pp_meth fmt t =
    ( match t with
    | `GET -> "GET"
    | `HEAD -> "HEAD"
    | `POST -> "POST"
    | `PUT -> "PUT"
    | `DELETE -> "DELETE"
    | `CONNECT -> "CONNECT"
    | `OPTIONS -> "OPTIONS"
    | `TRACE -> "TRACE"
    | `OTHER s -> Format.sprintf "OTHER (%s)" s )
    |> Format.fprintf fmt "%s"

  and pp_headers fmt t =
    let header_field = Fmt.(pair ~sep:comma string string) in
    Fmt.(list ~sep:sp header_field) fmt t

  let rec parse client_addr connection_fd =
    let input = Reparse_lwt_unix.Fd.create_input connection_fd in
    Lwt_result.(
      Parser.(parse input request_line)
      >>= fun (meth, request_target, (major, minor)) ->
      let meth = parse_meth meth in
      ( if Char.equal major '1' && Char.equal minor '1' then return (1, 1)
      else fail "Invalid HTTP version" )
      >>= fun http_version ->
      Parser.(parse input header_fields)
      >|= fun headers ->
      {meth; request_target; http_version; headers; client_addr; connection_fd})

  and parse_meth meth =
    String.uppercase_ascii meth
    |> function
    | "GET" -> `GET
    | "HEAD" -> `HEAD
    | "POST" -> `POST
    | "PUT" -> `PUT
    | "DELETE" -> `DELETE
    | "CONNECT" -> `CONNECT
    | "OPTIONS" -> `OPTIONS
    | "TRACE" -> `TRACE
    | header -> `OTHER header
end

type request_handler = Request.t -> unit Lwt.t

let handle_connection request_handler client_addr fd =
  Lwt.(
    Request.parse client_addr fd
    >>= function
    | Ok t -> request_handler t
    | Error _e ->
        let response_txt = "400 Bad Request" in
        Lwt_unix.write_string fd response_txt 0 (String.length response_txt)
        >|= fun _ -> ())

let start port handler =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt_engine.set (new Lwt_engine.libev ()) ;
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket ~backlog:11_000 listen_address
        (handle_connection handler)
      >>= fun _server -> Lwt.return () ) ;
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
