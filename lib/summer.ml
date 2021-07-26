(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

let _debug_on =
  ref
    ( match String.trim @@ Sys.getenv "HTTP_DBG" with
    | "" -> false
    | _ -> true
    | exception _ -> false )

let _enable_debug b = _debug_on := b

let _debug k =
  if !_debug_on then
    k (fun fmt ->
        Printf.kfprintf (fun oc -> Printf.fprintf oc "\n%!") stdout fmt )

type request =
  { meth: meth
  ; request_target: string
  ; http_version: int * int
  ; headers: (string, string) Hashtbl.t
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

and error = string

and body_reader =
  { input: Reparse_lwt_unix.Fd.input
  ; mutable pos: Reparse_lwt_unix.Fd.pos
  ; mutable total_read: int }

and content_length = int

and boundary = Http_multipart_formdata.boundary

(*-- Handler and context --*)
and 'a handler = context -> 'a Lwt.t

and context =
  { conn: Lwt_unix.file_descr
  ; request: request
  ; response_headers: (string, string) Hashtbl.t }

(*-- Request --*)

module Make_common (P : Reparse.PARSER) = struct
  open P

  let pair a b = (a, b)

  let _peek_dbg n =
    let+ s = peek_string n in
    _debug (fun k -> k "peek: %s\n%!" s)

  (*-- https://datatracker.ietf.org/doc/html/rfc7230#appendix-B --*)
  let tchar =
    char_if (function
      | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
       |'`' | '|' | '~' ->
          true
      | _ -> false )
    <|> digit <|> alpha

  let token = take ~at_least:1 tchar >>= string_of_chars
  let ows = skip (space <|> htab) *> unit

  (*-- https://datatracker.ietf.org/doc/html/rfc7230#section-3.2 --*)
  let header_fields =
    let header_field =
      let* field_name = token <* char ':' <* ows >>| String.lowercase_ascii in
      let+ field_value =
        let field_content =
          let c2 =
            optional
              (let* c1 = skip ~at_least:1 (space <|> htab) *> vchar in
               string_of_chars [' '; c1] )
            >>| function Some s -> s | None -> ""
          in
          (vchar, c2) <$$> fun c1 c2 -> Format.sprintf "%c%s" c1 c2
        in
        take field_content >>| String.concat "" <* crlf <* trim_input_buffer
      in
      (field_name, field_value)
    in
    take header_field
end

module Option = struct
  include Option

  let ( >>= ) b f = Option.bind b f [@@warning "-32"]
  let ( >>| ) b f = Option.map f b [@@warning "-32"]
  let ( let* ) = ( >>= ) [@@warning "-32"]
  let ( let+ ) = ( >>| ) [@@warning "-32"]
end

module Result = struct
  include Result

  let ( >>= ) b f = Result.bind b f [@@warning "-32"]
  let ( >>| ) b f = Result.map f b [@@warning "-32"]
  let ( let* ) = ( >>= ) [@@warning "-32"]
  let ( let+ ) = ( >>| ) [@@warning "-32"]
end

module C = struct
  let content_length = "content-length"
end

let meth t = t.meth
let target t = t.request_target
let http_version t = t.http_version
let headers t = Hashtbl.to_seq t.headers |> List.of_seq
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
  Fmt.vbox Fmt.(list header_field) fmt (Hashtbl.to_seq t |> List.of_seq)

let show_request t =
  let buf = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buf in
  pp_request fmt t ; Format.fprintf fmt "%!" ; Buffer.contents buf

let content_length request =
  match Hashtbl.find_opt request.headers C.content_length with
  | Some content_length -> (
    try Ok (int_of_string content_length)
    with _ ->
      Error
        (Format.sprintf "Invalid '%s' value: %s" C.content_length content_length)
    )
  | None -> Error (Format.sprintf "%s header not found" C.content_length)

(*-- request-line = method SP request-target SP HTTP-version CRLF -- *)
let request_line =
  let open Reparse_lwt_unix.Fd in
  let open Make_common (Reparse_lwt_unix.Fd) in
  let* meth = token <* space in
  let* request_target =
    take_while ~while_:(is_not space) unsafe_any_char
    >>= string_of_chars <* space
  in
  let* http_version =
    (*-- https://datatracker.ietf.org/doc/html/rfc7230#section-2.6 --*)
    (string_cs "HTTP/" *> digit <* char '.', digit)
    <$$> pair <* crlf
    >>= fun (major, minor) ->
    if Char.equal major '1' && Char.equal minor '1' then return (1, 1)
    else Format.sprintf "Invalid HTTP version: (%c,%c)" major minor |> fail
  in
  trim_input_buffer *> return (meth, request_target, http_version)

let request_meta =
  let open Reparse_lwt_unix.Fd in
  let open Make_common (Reparse_lwt_unix.Fd) in
  (request_line, header_fields) <$$> pair <* crlf

let rec create_request (client_addr : Lwt_unix.sockaddr) fd =
  let open Reparse_lwt_unix.Fd in
  let open Make_common (Reparse_lwt_unix.Fd) in
  let input = Reparse_lwt_unix.Fd.create_input fd in
  Lwt_result.(
    parse input request_meta
    >|= fun ((request_line, headers), _) ->
    let meth, request_target, http_version = request_line in
    let meth = parse_meth meth in
    { meth
    ; request_target
    ; http_version
    ; headers= List.to_seq headers |> Hashtbl.of_seq
    ; client_addr })

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
  | header -> `Method header

(*-- Request --*)

let request ctx = ctx.request
let conn ctx = ctx.conn

let body_reader context =
  let input = Reparse_lwt_unix.Fd.create_input context.conn in
  {input; pos= 0; total_read= 0}

let read_content content_length ?(read_buf_size = content_length) reader
    _context =
  let content_parser reader content_length =
    Reparse_lwt_unix.Fd.(
      if reader.total_read < content_length then (
        let total_unread = content_length - reader.total_read in
        let read_buf_size =
          if read_buf_size > total_unread then total_unread else read_buf_size
        in
        let+ buf = unsafe_take_cstruct_ne read_buf_size <* trim_input_buffer in
        reader.total_read <- reader.total_read + Cstruct.length buf ;
        `Content buf )
      else return `End)
  in
  Reparse_lwt_unix.Fd.(
    Lwt.(
      parse ~pos:reader.pos reader.input (content_parser reader content_length)
      >>= function
      | Ok (x, pos) ->
          reader.pos <- pos ;
          return x
      | Error e -> return (`Error e)))

open Lwt.Infix
module IO_vector = Lwt_unix.IO_vectors

let respond_with_bigstring ~(status_code : int) ~(reason_phrase : string)
    ~(content_type : string) (body : Cstruct.buffer) context =
  let iov = IO_vector.create () in
  let status_line =
    Format.sprintf "HTTP/1.1 %d %s\r\n" status_code reason_phrase
    |> Lwt_bytes.of_string
  in
  IO_vector.append_bigarray iov status_line 0 (Lwt_bytes.length status_line) ;
  let content_type_header =
    Format.sprintf "Content-Type: %s\r\n" content_type |> Lwt_bytes.of_string
  in
  IO_vector.append_bigarray iov content_type_header 0
    (Lwt_bytes.length content_type_header) ;
  let content_length = Lwt_bytes.length body in
  let content_length_header =
    Format.sprintf "Content-Length: %d\r\n" content_length
    |> Lwt_bytes.of_string
  in
  IO_vector.append_bigarray iov content_length_header 0
    (Lwt_bytes.length content_length_header) ;
  IO_vector.append_bytes iov (Bytes.unsafe_of_string "\r\n") 0 2 ;
  IO_vector.append_bigarray iov body 0 content_length ;
  Lwt_unix.writev context.conn iov >|= fun _ -> ()

let write_status conn status_code reason_phrase =
  let iov = IO_vector.create () in
  let status_line =
    Format.sprintf "HTTP/1.1 %d %s\r\n" status_code reason_phrase
    |> Lwt_bytes.of_string
  in
  IO_vector.append_bigarray iov status_line 0 (Lwt_bytes.length status_line) ;
  Lwt_unix.writev conn iov >|= fun _ -> ()

let rec handle_requests request_handler client_addr conn =
  _debug (fun k -> k "Waiting for new request ...\n%!") ;
  create_request client_addr conn
  >>= function
  | Ok req -> (
      _debug (fun k -> k "%s\n%!" (show_request req)) ;
      Lwt.catch
        (fun () ->
          let context =
            {conn; request= req; response_headers= Hashtbl.create 0}
          in
          request_handler context )
        (fun exn ->
          _debug (fun k -> k "Unhandled exception: %s" (Printexc.to_string exn)) ;
          write_status conn 500 "Internal Server Error" )
      >>= fun () ->
      match Hashtbl.find_opt req.headers "Connection" with
      | Some "close" -> Lwt_unix.close conn
      | Some _ | None -> handle_requests request_handler client_addr conn )
  | Error e ->
      _debug (fun k -> k "Error: %s\n\nClosing connection." e) ;
      Lwt_unix.close conn

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
