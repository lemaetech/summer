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

type request =
  { meth: meth
  ; request_target: string
  ; http_version: int * int
  ; content_length: int option
  ; headers: (string * string) list
  ; client_addr: string (* IP address of client. *)
  ; fd: Lwt_unix.file_descr
  ; (* unconsumed - bytes remaining after request is processed *)
    mutable unconsumed: Cstruct.t
  ; (* body_read - denotes if the request body has been read or not. This is used
           to determine if the connection socket needs to be drained before
           reading another request in the same connection. *)
    mutable body_read: bool
  ; mutable multipart_reader: Http_multipart_formdata.reader option }

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

exception Request_error of string

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

let content_length request = request.content_length

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

let socketaddr_to_string = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (inet, port) ->
      Format.sprintf "%s:%d" (Unix.string_of_inet_addr inet) port

let request fd unconsumed client_addr =
  let request_or_eof =
    let request' =
      (let* meth, request_target, http_version = request_line in
       let* headers = header_fields in
       let+ content_length =
         match List.assoc_opt "content-length" headers with
         | Some len -> (
           try return (Some (int_of_string len))
           with _ ->
             raise
               (Request_error
                  (Format.sprintf "Invalid content-length value: %s" len) ) )
         | None -> return None
       in
       `Request (meth, request_target, http_version, content_length, headers) )
      <* crlf
    in
    let eof = end_of_input >>| fun () -> `Connection_closed in
    request' <|> eof
  in
  let open Lwt.Syntax in
  let rec parse_request parse_state =
    match parse_state with
    | Buffered.Partial k ->
        let unconsumed_length = Cstruct.length unconsumed in
        let len = io_buffer_size - unconsumed_length in
        let buf = Cstruct.create len in
        let* len' = Lwt_bytes.read fd buf.buffer 0 len in
        if len' = 0 then parse_request (k `Eof)
        else
          let buf = if len' != len then Cstruct.sub buf 0 len' else buf in
          let bigstring =
            ( if unconsumed_length > 0 then Cstruct.(append unconsumed buf)
            else buf )
            |> Cstruct.to_bigarray
          in
          parse_request (k (`Bigstring bigstring))
    | Buffered.Done ({off; len; buf}, x) ->
        let unconsumed =
          if len > 0 then Cstruct.of_bigarray ~off ~len buf else Cstruct.empty
        in
        Lwt.return (Ok (x, unconsumed))
    | Buffered.Fail (_, marks, err) ->
        Lwt.return (Error (String.concat " > " marks ^ ": " ^ err))
  in
  let+ parse_result = parse_request (Buffered.parse request_or_eof) in
  match parse_result with
  | Ok (x, unconsumed) -> (
    match x with
    | `Request (meth, request_target, http_version, content_length, headers) ->
        `Request
          { meth
          ; request_target
          ; http_version
          ; content_length
          ; headers
          ; client_addr= socketaddr_to_string client_addr
          ; fd
          ; body_read= false
          ; unconsumed
          ; multipart_reader= None }
    | `Connection_closed -> `Connection_closed )
  | Error x -> `Error x

open Lwt.Infix
open Lwt.Syntax

let body request =
  match content_length request with
  | Some content_length ->
      let unconsumed_length = Cstruct.length request.unconsumed in
      if content_length = 0 || request.body_read then (
        request.body_read <- true ;
        Lwt.return "" )
      else if content_length = unconsumed_length then (
        request.body_read <- true ;
        Lwt.return (Cstruct.to_string request.unconsumed) )
      else if content_length < unconsumed_length then (
        let sz = unconsumed_length - content_length in
        let buf =
          Cstruct.(sub request.unconsumed 0 content_length |> to_string)
        in
        let unconsumed = Cstruct.sub request.unconsumed content_length sz in
        request.unconsumed <- unconsumed ;
        request.body_read <- true ;
        Lwt.return buf )
      else
        let sz = content_length - unconsumed_length in
        let buf = Cstruct.create sz in
        let+ sz' = Lwt_bytes.read request.fd buf.buffer 0 sz in
        let buf = if sz' <> sz then Cstruct.sub buf 0 sz' else buf in
        request.body_read <- true ;
        ( if unconsumed_length > 0 then Cstruct.append request.unconsumed buf
        else buf )
        |> Cstruct.to_string
  | None -> raise (Request_error "content-length header not found")

(* Form *)

let multipart ?(body_buffer_size = io_buffer_size) request =
  let body_buffer_size =
    if body_buffer_size > io_buffer_size then io_buffer_size
    else body_buffer_size
  in
  let rec parse_part = function
    | `End ->
        let unconsumed =
          Http_multipart_formdata.unconsumed
            (Option.get request.multipart_reader)
        in
        request.unconsumed <- unconsumed ;
        request.body_read <- true ;
        request.multipart_reader <- None ;
        Lwt.return `End
    | `Header _ as part_header -> Lwt.return part_header
    | `Body _ as body -> Lwt.return body
    | `Body_end -> Lwt.return `Body_end
    | `Awaiting_input k ->
        let buf = Cstruct.create io_buffer_size in
        let* len' = Lwt_bytes.read request.fd buf.buffer 0 io_buffer_size in
        let buf =
          if len' <> io_buffer_size then Cstruct.sub buf 0 len' else buf
        in
        let buf =
          if Cstruct.length request.unconsumed > 0 then (
            let buf = Cstruct.append request.unconsumed buf in
            request.unconsumed <- Cstruct.empty ;
            buf )
          else buf
        in
        parse_part (k (`Cstruct buf))
    | `Error error -> raise (Request_error error)
  in
  match request.multipart_reader with
  | Some reader -> parse_part (Http_multipart_formdata.read reader)
  | None ->
      let boundary =
        match List.assoc_opt "content-type" request.headers with
        | Some ct -> (
          match Http_multipart_formdata.boundary ct with
          | Ok boundary -> boundary
          | Error err -> raise (Request_error err) )
        | None ->
            raise (Request_error "[multipart] content-type header not found")
      in
      let reader =
        Http_multipart_formdata.reader ~read_buffer_size:body_buffer_size
          boundary `Incremental
      in
      request.multipart_reader <- Some reader ;
      parse_part (Http_multipart_formdata.read reader)

let multipart_all request =
  let rec read_parts parts =
    multipart request
    >>= function
    | `End -> Lwt.return (List.rev parts)
    | `Header header ->
        let* body = read_body Cstruct.empty in
        read_parts ((header, body) :: parts)
    | `Error e -> raise (Request_error e)
    | _ -> assert false
  and read_body body =
    multipart request
    >>= function
    | `Body_end -> Lwt.return body
    | `Body buf -> read_body (Cstruct.append body buf)
    | `Error e -> raise (Request_error e)
    | _ -> assert false
  in
  read_parts []

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
let response_code_200 = response_code 200
let response_code_500 = response_code 500
let response_code_400 = response_code 400

module Smap = Map.Make (String)

type response =
  { response_code: response_code
  ; headers: string Smap.t
  ; cookies: Http_cookie.t Smap.t
  ; body: Cstruct.t }

(*-- Handler --*)
type handler = request -> response Lwt.t

let response_bigstring ?(response_code = response_code_200) ?(headers = []) body
    =
  { response_code
  ; headers=
      List.map
        (fun (name, value) -> (String.lowercase_ascii name, value))
        headers
      |> List.to_seq |> Smap.of_seq
  ; cookies= Smap.empty
  ; body= Cstruct.of_bigarray body }

let response ?response_code ?headers body =
  response_bigstring ?response_code ?headers
    (Bigstringaf.of_string body ~off:0 ~len:(String.length body))

(* Cookies *)

let add_cookie cookie response =
  { response with
    cookies=
      Smap.update (Http_cookie.name cookie)
        (fun _ -> Some cookie)
        response.cookies }

let remove_cookie cookie_name response =
  let cookies = Smap.remove cookie_name response.cookies in
  {response with cookies}

let add_header ~name value response =
  { response with
    headers= Smap.update name (fun _ -> Some value) response.headers }

let remove_header name response =
  {response with headers= Smap.remove name response.headers}

module IO_vector = Lwt_unix.IO_vectors

let write_response fd {response_code; headers; body; cookies} =
  let iov = IO_vector.create () in

  (* Write response status line. *)
  let status_line =
    Format.sprintf "HTTP/1.1 %d %s\r\n"
      (response_code_int response_code)
      (response_code_reason_phrase response_code)
    |> Bytes.unsafe_of_string
  in
  IO_vector.append_bytes iov status_line 0 (Bytes.length status_line) ;

  (* Write response headers. *)
  ( if Smap.mem "content-length" headers then headers
  else
    let len = Cstruct.length body |> string_of_int in
    Smap.add "content-length" len headers )
  |> Smap.iter (fun name v ->
         let buf =
           Format.sprintf "%s: %s\r\n" name v |> Bytes.unsafe_of_string
         in
         IO_vector.append_bytes iov buf 0 (Bytes.length buf) ) ;

  (* Write Set-Cookie headers. *)
  Smap.iter
    (fun _ cookie ->
      let header =
        Format.sprintf "set-cookie: %s"
          (Http_cookie.to_cookie_header_value cookie)
        |> Bytes.unsafe_of_string
      in
      IO_vector.append_bytes iov header 0 (Bytes.length header) )
    cookies ;

  (* Write response body. *)
  IO_vector.append_bytes iov (Bytes.unsafe_of_string "\r\n") 0 2 ;
  if Cstruct.length body > 0 then
    IO_vector.append_bigarray iov (Cstruct.to_bigarray body) 0
      (Cstruct.length body) ;

  Lwt_unix.writev fd iov >|= fun _ -> ()

let rec handle_requests unconsumed handler client_addr fd =
  _debug (fun k -> k "Waiting for new request ...\n%!") ;
  request fd unconsumed client_addr
  >>= function
  | `Request req -> (
      _debug (fun k -> k "%s\n%!" (show_request req)) ;
      Lwt.catch
        (fun () ->
          let* response = handler req in
          write_response fd response
          >>= fun () ->
          match List.assoc_opt "Connection" req.headers with
          | Some "close" -> Lwt.return `Close_connection
          | Some _ | None ->
              (* Drain request body content (bytes) from fd before reading a new request
                 in the same connection. *)
              if (not req.body_read) && Option.is_some req.content_length then
                body req >|= fun _ -> `Next_request
              else Lwt.return `Next_request )
        (fun exn ->
          ( match exn with
          | Request_error error ->
              _debug (fun k -> k "Request error: %s" error) ;
              write_response fd (response ~response_code:response_code_400 "")
          | exn ->
              _debug (fun k -> k "Exception: %s" (Printexc.to_string exn)) ;
              write_response fd (response ~response_code:response_code_500 "")
          )
          >|= fun () -> `Close_connection )
      >>= function
      | `Close_connection -> Lwt_unix.close fd
      | `Next_request -> handle_requests req.unconsumed handler client_addr fd )
  | `Connection_closed ->
      _debug (fun k -> k "Client closed connection") ;
      Lwt_unix.close fd
  | `Error e ->
      _debug (fun k ->
          k "Error while parsing request: %s\nClosing connection" e ) ;
      write_response fd (response ~response_code:response_code_400 "")
      >>= fun () -> Lwt_unix.close fd

let start ~port request_handler =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt_engine.set (new Lwt_engine.libev ()) ;
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket ~backlog:11_000 ~no_close:true
        listen_address
        (handle_requests Cstruct.empty request_handler)
      >>= fun _server -> Lwt.return () ) ;
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
