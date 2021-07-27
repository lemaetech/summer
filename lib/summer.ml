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

let _peek_dbg n =
  let+ s = peek_string n in
  _debug (fun k -> k "peek: %s\n%!" (String.escaped s))

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

(*-- Handler and context --*)
and 'a handler = context -> request -> 'a Lwt.t

and context = {fd: Lwt_unix.file_descr; mutable unconsumed: Cstruct.t option}

and request_body =
  | Partial of {body: Cstruct.t; continue: unit -> request_body Lwt.t}
  | Done

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
  let content_length = "content-length" in
  match Hashtbl.find_opt request.headers content_length with
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

let request context client_addr =
  let p =
    (let* meth, request_target, http_version = request_line in
     let+ headers = header_fields in
     { meth
     ; request_target
     ; http_version
     ; headers= List.to_seq headers |> Hashtbl.of_seq
     ; client_addr } )
    <* crlf
  in
  let unix_buffer_size = 65536 (* UNIX_BUFFER_SIZE 4.0.0 *) in
  let open Lwt.Syntax in
  let rec read reader = function
    | Buffered.Partial k ->
        let len =
          unix_buffer_size
          -
          match context.unconsumed with Some b -> Cstruct.length b | None -> 0
        in
        let buf = Cstruct.create len in
        let* len' = Lwt_bytes.read context.fd buf.buffer 0 len in
        if len' = 0 then read reader (k `Eof)
        else if len' != len then (
          let buf' = Cstruct.create len' in
          Cstruct.blit buf 0 buf' 0 len' ;
          let buf =
            match context.unconsumed with
            | Some b -> Cstruct.(append b buf')
            | None -> buf'
          in
          read reader (k (`Bigstring (Cstruct.to_bigarray buf))) )
        else read reader (k (`Bigstring (Cstruct.to_bigarray buf)))
    | Buffered.Done ({off; len; buf}, x) ->
        if len > 0 then
          reader.unconsumed <- Some (Cstruct.of_bigarray ~off ~len buf)
        else reader.unconsumed <- None ;
        Lwt.return (Ok x)
    | Buffered.Fail ({off; len; buf}, marks, err) ->
        if len > 0 then
          reader.unconsumed <- Some (Cstruct.of_bigarray ~off ~len buf)
        else reader.unconsumed <- None ;
        Lwt.return (Error (String.concat " > " marks ^ ": " ^ err))
  in
  read context (Buffered.parse p)

let rec read fd unconsumed ~content_length ~total_read ~read_buffer_size =
  let open Lwt.Syntax in
  if total_read < content_length then
    let total_unread = content_length - total_read in
    let buffer_size =
      if read_buffer_size > total_unread then total_unread else read_buffer_size
    in
    let buf = Cstruct.create buffer_size in
    let* len' = Lwt_bytes.read fd buf.buffer 0 buffer_size in
    let continue () =
      read fd None ~content_length ~total_read:(total_read + len')
        ~read_buffer_size
    in
    if len' = 0 then Lwt.return Done
    else if len' != buffer_size then (
      let buf' = Cstruct.create len' in
      Cstruct.blit buf 0 buf' 0 len' ;
      let buf =
        match unconsumed with Some b -> Cstruct.(append b buf) | None -> buf'
      in
      Lwt.return (Partial {body= buf; continue}) )
    else Lwt.return (Partial {body= buf; continue})
  else Lwt.return Done

let request_body ?(read_buffer_size = 1024) ~content_length context =
  read context.fd context.unconsumed ~content_length ~total_read:0
    ~read_buffer_size

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
  Lwt_unix.writev context.fd iov >|= fun _ -> ()

let write_status conn status_code reason_phrase =
  let iov = IO_vector.create () in
  let status_line =
    Format.sprintf "HTTP/1.1 %d %s\r\n" status_code reason_phrase
    |> Lwt_bytes.of_string
  in
  IO_vector.append_bigarray iov status_line 0 (Lwt_bytes.length status_line) ;
  Lwt_unix.writev conn iov >|= fun _ -> ()

let rec handle_requests request_handler client_addr fd =
  _debug (fun k -> k "Waiting for new request ...\n%!") ;
  let context = {fd; unconsumed= None} in
  request context client_addr
  >>= function
  | Ok req -> (
      _debug (fun k -> k "%s\n%!" (show_request req)) ;
      Lwt.catch
        (fun () -> request_handler context req)
        (fun exn ->
          _debug (fun k -> k "Unhandled exception: %s" (Printexc.to_string exn)) ;
          write_status fd 500 "Internal Server Error" )
      >>= fun () ->
      match Hashtbl.find_opt req.headers "Connection" with
      | Some "close" -> Lwt_unix.close fd
      | Some _ | None -> handle_requests request_handler client_addr fd )
  | Error e ->
      _debug (fun k -> k "Error: %s\n\nClosing connection." e) ;
      Lwt_unix.close fd

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
