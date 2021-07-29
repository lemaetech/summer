(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

open Angstrom

let _debug_on =
  ref
    ( match String.trim @@ Sys.getenv "HTTP_DBG" with
    | "" -> false
    | _ -> true
    | exception _ -> false )

let _debug k =
  if !_debug_on then
    k (fun fmt ->
        Printf.kfprintf (fun oc -> Printf.fprintf oc "\n%!") stdout fmt )

type t =
  { fd: Lwt_unix.file_descr
  ; mutable unconsumed: Cstruct.t
  ; mutable body_read: bool }

type request =
  { meth: meth
  ; request_target: string
  ; http_version: int * int
  ; headers: (string * string) list
  ; client_addr: Lwt_unix.sockaddr }

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
  | `Method of string ]

and header = string * string
(* (name,value) *)

let io_buffer_size = 65536 (* UNIX_BUFFER_SIZE 4.0.0 *)

let meth t = t.meth
let target t = t.request_target
let http_version t = t.http_version
let headers t = t.headers
let client_addr t = t.client_addr

let rec pp_request fmt t =
  let fields =
    [ Fmt.field "meth" (fun p -> p.meth) pp_meth
    ; Fmt.field "request_target" (fun p -> p.request_target) Fmt.string
    ; Fmt.field "http_version" (fun p -> p.http_version) pp_http_version
    ; Fmt.field "headers" (fun p -> p.headers) pp_headers ]
  in
  Fmt.record fields fmt t

and pp_http_version fmt t =
  let comma' fmt _ = Fmt.string fmt "," in
  Fmt.(pair ~sep:comma' int int) fmt t

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
  | `Method s -> Format.sprintf "Method (%s)" s )
  |> Format.fprintf fmt "%s"

and pp_headers fmt t =
  let colon fmt _ = Fmt.string fmt ": " in
  let header_field = Fmt.(pair ~sep:colon string string) in
  Fmt.vbox Fmt.(list header_field) fmt t

let show_request t =
  let buf = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buf in
  pp_request fmt t ; Format.fprintf fmt "%!" ; Buffer.contents buf

let content_length request =
  match List.assoc_opt "content-length" request.headers with
  | Some len -> ( try int_of_string len with _ -> 0 )
  | None -> 0

let method_ meth =
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
  | header -> `Method header

(*-- https://datatracker.ietf.org/doc/html/rfc7230#appendix-B --*)

let token =
  take_while1 (function
    | '0' .. '9'
     |'a' .. 'z'
     |'A' .. 'Z'
     |'!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
     |'`' | '|' | '~' ->
        true
    | _ -> false )

let space = char '\x20'
let htab = char '\t'
let ows = skip_many (space <|> htab)
let optional x = option None (x >>| Option.some)
let vchar = satisfy (function '\x21' .. '\x7E' -> true | _ -> false)
let crlf = string_ci "\r\n" <?> "[crlf]"

(*-- https://datatracker.ietf.org/doc/html/rfc7230#section-3.2 --*)
let header_fields =
  let header_field =
    let* field_name = token <* char ':' <* ows >>| String.lowercase_ascii in
    let+ field_value =
      let field_content =
        let c2 =
          optional
            (let+ c1 = skip_many1 (space <|> htab) *> vchar in
             Format.sprintf " %c" c1 )
          >>| function Some s -> s | None -> ""
        in
        lift2 (fun c1 c2 -> Format.sprintf "%c%s" c1 c2) vchar c2
      in
      many field_content >>| String.concat "" <* crlf <* commit
    in
    (field_name, field_value)
  in
  many header_field

(*-- request-line = method SP request-target SP HTTP-version CRLF -- *)
let request_line =
  let* meth = token >>| method_ <* space in
  let* request_target = take_while1 (fun c -> c != ' ') <* space in
  let digit = satisfy (function '0' .. '9' -> true | _ -> false) in
  let* http_version =
    let* major = string "HTTP/" *> digit <* char '.' in
    let* minor = digit <* crlf in
    if Char.equal major '1' && Char.equal minor '1' then return (1, 1)
    else fail (Format.sprintf "Invalid HTTP version: (%c,%c)" major minor)
  in
  commit *> return (meth, request_target, http_version)

let request t client_addr =
  let request_or_eof =
    let request' =
      (let* meth, request_target, http_version = request_line in
       let+ headers = header_fields in
       `Request {meth; request_target; http_version; headers; client_addr} )
      <* crlf
    in
    let eof = end_of_input >>| fun () -> `End in
    request' <|> eof
  in
  let open Lwt.Syntax in
  let rec read reader = function
    | Buffered.Partial k ->
        let len = io_buffer_size - Cstruct.length t.unconsumed in
        let buf = Cstruct.create len in
        let* len' = Lwt_bytes.read t.fd buf.buffer 0 len in
        if len' = 0 then read reader (k `Eof)
        else if len' != len then (
          let buf' = Cstruct.create len' in
          Cstruct.blit buf 0 buf' 0 len' ;
          let buf'' = Cstruct.(append t.unconsumed buf') in
          read reader (k (`Bigstring (Cstruct.to_bigarray buf''))) )
        else read reader (k (`Bigstring (Cstruct.to_bigarray buf)))
    | Buffered.Done ({off; len; buf}, x) ->
        if len > 0 then reader.unconsumed <- Cstruct.of_bigarray ~off ~len buf
        else reader.unconsumed <- Cstruct.empty ;
        Lwt.return (Ok x)
    | Buffered.Fail ({off; len; buf}, marks, err) ->
        if len > 0 then reader.unconsumed <- Cstruct.of_bigarray ~off ~len buf
        else reader.unconsumed <- Cstruct.empty ;
        Lwt.return (Error (String.concat " > " marks ^ ": " ^ err))
  in
  let+ read_result = read t (Buffered.parse request_or_eof) in
  match read_result with Ok x -> x | Error x -> `Error x

open Lwt.Infix
open Lwt.Syntax

let read_body request t =
  let content_length = content_length request in
  let unconsumed_length = Cstruct.length t.unconsumed in
  if content_length = 0 || t.body_read then (
    t.body_read <- true ;
    Lwt.return Cstruct.empty )
  else if content_length = unconsumed_length then (
    t.body_read <- true ;
    Lwt.return t.unconsumed )
  else if content_length < unconsumed_length then (
    let sz = unconsumed_length - content_length in
    let buf = Cstruct.sub t.unconsumed 0 content_length in
    let unconsumed = Cstruct.sub t.unconsumed content_length sz in
    t.unconsumed <- unconsumed ;
    t.body_read <- true ;
    Lwt.return buf )
  else
    let sz = content_length - unconsumed_length in
    let buf = Cstruct.create sz in
    let+ sz' = Lwt_bytes.read t.fd buf.buffer 0 sz in
    let buf = if sz' <> sz then Cstruct.sub buf 0 sz' else buf in
    t.body_read <- true ;
    if unconsumed_length > 0 then Cstruct.append t.unconsumed buf else buf

(* Response *)

type response_code = int * string (* code, reason phrase *)

let response_code ?(reason_phrase = "unknown") = function
  (* Informational *)
  | 100 -> (100, "Continue")
  | 101 -> (101, "Switching Protocols")
  (* Successful *)
  | 200 -> (200, "OK")
  | 201 -> (201, "Created")
  | 202 -> (202, "Accepted")
  | 203 -> (203, "Non-Authoritative Information")
  | 204 -> (204, "No Content")
  | 205 -> (205, "Reset Content")
  | 206 -> (206, "Partial Content")
  (* Redirection *)
  | 300 -> (300, "Multiple Choices")
  | 301 -> (301, "Moved Permanently")
  | 302 -> (302, "Found")
  | 303 -> (303, "See Other")
  | 304 -> (304, "Not Modified")
  | 305 -> (305, "Use Proxy")
  | 306 -> (306, "Temporary Redirect")
  (* Client error *)
  | 400 -> (400, "Bad Request")
  | 401 -> (401, "Unauthorized")
  | 402 -> (402, "Payment Required")
  | 403 -> (403, "Forbidden")
  | 404 -> (404, "Not Found")
  | 405 -> (405, "Method Not Allowed")
  | 406 -> (406, "Not Acceptable")
  | 407 -> (407, "Proxy Authentication Required")
  | 408 -> (408, "Request Timeout")
  | 409 -> (409, "Conflict")
  | 410 -> (410, "Gone")
  | 411 -> (411, "Length Required")
  | 412 -> (412, "Precondition Failed")
  | 413 -> (413, "Payload Too Large")
  | 414 -> (414, "URI Too Long")
  | 415 -> (415, "Unsupported Media Type")
  | 416 -> (416, "Range Not Satisfiable")
  | 417 -> (417, "Expectation Failed")
  | 418 -> (418, "I'm a teapot") (* RFC 2342 *)
  | 420 -> (420, "Enhance Your Calm")
  | 426 -> (426, "Upgrade Required")
  (* Server error *)
  | 500 -> (500, "Internal Server Error")
  | 501 -> (501, "Not Implemented")
  | 502 -> (502, "Bad Gateway")
  | 503 -> (503, "Service Unavailable")
  | 504 -> (504, "Gateway Timeout")
  | 505 -> (505, "HTTP Version Not Supported")
  | c ->
      if c < 0 then failwith (Printf.sprintf "code: %d is negative" c)
      else if c < 100 || c > 999 then
        failwith (Printf.sprintf "code: %d is not a three-digit number" c)
      else (c, reason_phrase)

let response_code_int : response_code -> int = fun (code, _) -> code
let response_code_reason_phrase (_, phrase) = phrase
let response_code_ok = response_code 200

module IO_vector = Lwt_unix.IO_vectors

type response =
  {response_code: response_code; headers: header list; body: Cstruct.t}

(*-- Handler --*)
type handler = request -> response Lwt.t

let response_bigstring ?(response_code = response_code_ok) ?(headers = []) body
    =
  { response_code
  ; headers=
      List.map
        (fun (name, value) -> (String.lowercase_ascii name, value))
        headers
  ; body= Cstruct.of_bigarray body }

let response ?response_code ?headers body =
  response_bigstring ?response_code ?headers
    (Bigstringaf.of_string body ~off:0 ~len:(String.length body))

let headers_to_bytes headers =
  let buf = Buffer.create 0 in
  List.iter
    (fun (name, value) ->
      Format.sprintf "%s: %s\r\n" name value |> Buffer.add_string buf )
    headers ;
  (Buffer.length buf, Buffer.to_bytes buf)

let write_response t {response_code; headers; body} =
  let iov = IO_vector.create () in

  (* Append status line. *)
  let status_line =
    Format.sprintf "HTTP/1.1 %d %s\r\n"
      (response_code_int response_code)
      (response_code_reason_phrase response_code)
    |> Bytes.unsafe_of_string
  in
  IO_vector.append_bytes iov status_line 0 (Bytes.length status_line) ;

  (* Append headers. *)
  let headers =
    if List.mem_assoc "content-length" headers then headers
    else ("content-length", Cstruct.length body |> string_of_int) :: headers
  in
  let len, header_bytes = headers_to_bytes headers in
  IO_vector.append_bytes iov header_bytes 0 len ;

  IO_vector.append_bytes iov (Bytes.unsafe_of_string "\r\n") 0 2 ;
  IO_vector.append_bigarray iov (Cstruct.to_bigarray body) 0
    (Cstruct.length body) ;
  Lwt_unix.writev t.fd iov >|= fun _ -> ()

let write_status conn status_code reason_phrase =
  let iov = IO_vector.create () in
  let status_line =
    Format.sprintf "HTTP/1.1 %d %s\r\n" status_code reason_phrase
    |> Lwt_bytes.of_string
  in
  IO_vector.append_bigarray iov status_line 0 (Lwt_bytes.length status_line) ;
  Lwt_unix.writev conn iov >|= fun _ -> ()

let rec handle_requests (request_handler : t -> handler) client_addr fd =
  _debug (fun k -> k "Waiting for new request ...\n%!") ;
  let t = {fd; unconsumed= Cstruct.empty; body_read= false} in
  request t client_addr
  >>= function
  | `Request req -> (
      _debug (fun k -> k "%s\n%!" (show_request req)) ;
      Lwt.catch
        (fun () ->
          let* response = request_handler t req in
          write_response t response )
        (fun exn ->
          _debug (fun k -> k "Unhandled exception: %s" (Printexc.to_string exn)) ;
          write_status fd 500 "Internal Server Error" )
      >>= fun () ->
      match List.assoc_opt "Connection" req.headers with
      | Some "close" -> Lwt_unix.close fd
      | Some _ | None -> handle_requests request_handler client_addr fd )
  | `End ->
      _debug (fun k -> k "Closing connection") ;
      Lwt_unix.close fd
  | `Error e ->
      _debug (fun k -> k "Error: %s\nClosing connection" e) ;
      write_status fd 400 "Bad Request" >>= fun () -> Lwt_unix.close fd

let start ~port request_handler =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt_engine.set (new Lwt_engine.libev ()) ;
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket ~backlog:11_000 ~no_close:true
        listen_address
        (handle_requests request_handler)
      >>= fun _server -> Lwt.return () ) ;
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
