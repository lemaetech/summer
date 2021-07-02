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

type chunk_extension = {name: string; value: string option}
type header = string * string (* (name,value) *)

module Parser = Reparse_lwt_unix.Fd
open Parser

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
  <|> digit
  <|> alpha

let token = take ~at_least:1 tchar >>= string_of_chars
let ows = skip (space <|> htab) *> unit

(*-- https://datatracker.ietf.org/doc/html/rfc7230#section-3.2 --*)
let header_fields =
  let header_field =
    let* field_name = token <* char ':' <* ows in
    let+ field_value =
      let field_content =
        let c2 =
          optional
            (let* c1 = skip ~at_least:1 (space <|> htab) *> vchar in
             string_of_chars [' '; c1])
          >>| function Some s -> s | None -> "" in
        (vchar, c2) <$$> fun c1 c2 -> Format.sprintf "%c%s" c1 c2 in
      take field_content >>| String.concat "" <* crlf <* trim_input_buffer
    in
    (field_name, field_value) in
  take header_field

module Option = struct
  include Option

  let ( >>= ) b f = Option.bind b f [@@warning "-32"]
end

module C = struct
  let transfer_encoding = "Transfer-Encoding"
  let trailer = "Trailer"
  let content_length = "Content-Length"
end

module Request = struct
  type t =
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
    | `OTHER of string ]

  let meth t = t.meth
  let request_target t = t.request_target
  let http_version t = t.http_version
  let headers t = t.headers

  let body_type t =
    let chunk =
      Option.(
        List.assoc_opt C.transfer_encoding t.headers
        >>= fun encoding ->
        String.split_on_char ',' encoding
        |> List.map (fun tok -> String.trim tok)
        |> fun encodings ->
        if List.mem "chunked" encodings then Some `Chunked else None) in
    match chunk with
    | Some `Chunked -> `Chunked
    | None -> (
        List.assoc_opt "Content-Length" t.headers
        |> function Some _ -> `Content | None -> `None )

  let client_addr t = t.client_addr

  let rec pp fmt t =
    let fields =
      [ Fmt.field "meth" (fun p -> p.meth) pp_meth
      ; Fmt.field "request_target" (fun p -> p.request_target) Fmt.string
      ; Fmt.field "http_version" (fun p -> p.http_version) pp_http_version
      ; Fmt.field "headers" (fun p -> p.headers) pp_headers ] in
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
    | `OTHER s -> Format.sprintf "OTHER (%s)" s )
    |> Format.fprintf fmt "%s"

  and pp_headers fmt t =
    let colon fmt _ = Fmt.string fmt ": " in
    let header_field = Fmt.(pair ~sep:colon string string) in
    Fmt.vbox Fmt.(list header_field) fmt t

  let show t =
    let buf = Buffer.create 0 in
    let fmt = Format.formatter_of_buffer buf in
    pp fmt t ; Format.fprintf fmt "%!" ; Buffer.contents buf

  (*-- request-line = method SP request-target SP HTTP-version CRLF -- *)
  let request_line =
    let* meth = token <* space in
    let* request_target =
      take_while ~while_:(is_not space) unsafe_any_char
      >>= string_of_chars
      <* space
    in
    let* http_version =
      (*-- https://datatracker.ietf.org/doc/html/rfc7230#section-2.6 --*)
      (string_cs "HTTP/" *> digit <* char '.', digit)
      <$$> pair
      <* crlf
      >>= fun (major, minor) ->
      if Char.equal major '1' && Char.equal minor '1' then return (1, 1)
      else Format.sprintf "Invalid HTTP version: (%c,%c)" major minor |> fail
    in
    trim_input_buffer *> return (meth, request_target, http_version)

  let request_meta = (request_line, header_fields) <$$> pair <* crlf

  let rec t (client_addr : Lwt_unix.sockaddr) fd =
    let input = Reparse_lwt_unix.Fd.create_input fd in
    Lwt_result.(
      Parser.(parse input request_meta)
      >|= fun (request_line, headers) ->
      let meth, request_target, http_version = request_line in
      let meth = parse_meth meth in
      {meth; request_target; http_version; headers; client_addr})

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

let read_body_chunks ~conn (Request.{headers; _} as req) ~on_chunk =
  let quoted_pair = char '\\' *> (whitespace <|> vchar) in
  (*-- qdtext = HTAB / SP /%x21 / %x23-5B / %x5D-7E / obs-text -- *)
  let qdtext =
    htab
    <|> space
    <|> char '\x21'
    <|> char_if (function
          | '\x23' .. '\x5B' -> true
          | '\x5D' .. '\x7E' -> true
          | _ -> false ) in
  (*-- quoted-string  = DQUOTE *( qdtext / quoted-pair ) DQUOTE --*)
  let quoted_string =
    take_between ~start:(char '"')
      (take (qdtext <|> quoted_pair) >>= string_of_chars)
      ~end_:(char '"')
    >>| String.concat "" in
  (*-- https://datatracker.ietf.org/doc/html/rfc7230#section-4.1 --*)
  let chunk_ext =
    let chunk_ext_name = token in
    let chunk_ext_val = quoted_string <|> token in
    take
      ( (char ';' *> chunk_ext_name, optional (char '=' *> chunk_ext_val))
      <$$> fun name value : chunk_extension -> {name; value} )
    <?> "[chunk_ext]" in
  let chunk_size =
    take ~at_least:1 hex_digit
    >>= string_of_chars
    >>= fun sz ->
    try return (Format.sprintf "0x%s" sz |> int_of_string)
    with _ -> fail (Format.sprintf "[chunk_size] Invalid chunk_size: %s" sz)
  in
  let trailer_part request_headers =
    let allowed_trailers =
      List.assoc_opt "Trailer" request_headers
      |> function
      | Some v -> String.split_on_char ',' v |> List.map String.trim
      | None -> [] in
    let+ headers = header_fields in
    List.filter (fun (name, _) -> List.mem name allowed_trailers) headers in
  let chunk_data n =
    let+ buf = unsafe_take_cstruct n <* trim_input_buffer in
    Cstruct.to_bigarray buf in
  (* Chunk decoding algorithm is explained at
     https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.3
  *)
  let chunked_body headers ~on_chunk =
    let content_length = ref 0 in
    let* () =
      let p = (chunk_size, chunk_ext <* crlf) <$$> pair in
      let continue = ref true in
      take_while_cb p ~while_:(return !continue) ~on_take_cb:(fun (sz, exts) ->
          _debug (fun k -> k "[chunked_body] sz: %d\n%!" sz) ;
          _debug (fun k -> k "[chunked_body] exts: %d\n%!" (List.length exts)) ;
          if sz > 0 then (
            content_length := !content_length + sz ;
            let* chunk_data' = chunk_data sz in
            _debug (fun k ->
                k "[chunked_body] chunk_data: %d\n%!"
                  (Lwt_bytes.length chunk_data') ) ;
            on_chunk ~chunk:chunk_data' ~len:sz exts |> of_promise )
          else (
            continue := false ;
            unit ) )
    in
    let remove_chunked headers =
      let te =
        List.assoc C.transfer_encoding headers
        |> String.split_on_char ','
        |> List.map String.trim
        |> List.filter (fun e -> not (String.equal e "chunked"))
        |> String.concat "," in
      let headers = List.remove_assoc C.transfer_encoding headers in
      if String.length te > 0 then (C.transfer_encoding, te) :: headers
      else headers in
    let+ trailer_headers = trailer_part headers <* crlf <* trim_input_buffer in
    _debug (fun k ->
        k "[chunked_body] trailer_headers: %d\n%!" (List.length trailer_headers) ) ;
    let headers =
      List.remove_assoc C.trailer headers
      |> remove_chunked
      |> List.append trailer_headers
      |> List.cons (C.content_length, string_of_int !content_length) in
    ({req with headers} : Request.t) in
  let input = Reparse_lwt_unix.Fd.create_input conn in
  chunked_body headers ~on_chunk |> Parser.parse input

let read_body_content ~conn req =
  List.assoc_opt "Content-Length" (Request.headers req)
  |> function
  | Some len -> (
      Lwt.(
        try
          let len = int_of_string len in
          let buf = Lwt_bytes.create len in
          Lwt_bytes.read conn buf 0 len >>= fun _ -> return (Ok buf)
        with _ ->
          Format.sprintf "[read_body_content] Invalid content-length: %s" len
          |> Lwt_result.fail) )
  | None ->
      Lwt_result.fail "[read_body_content] Content-Length header not found"

type request_handler = conn:Lwt_unix.file_descr -> Request.t -> unit Lwt.t
type bigstring = Lwt_bytes.t

open Lwt.Infix
module IO_vector = Lwt_unix.IO_vectors

let respond_with_bigstring ~conn ~(status_code : int) ~(reason_phrase : string)
    ~(content_type : string) (body : bigstring) =
  let iov = IO_vector.create () in
  let status_line =
    Format.sprintf "HTTP/1.1 %d %s\r\n" status_code reason_phrase
    |> Lwt_bytes.of_string in
  IO_vector.append_bigarray iov status_line 0 (Lwt_bytes.length status_line) ;
  let content_type_header =
    Format.sprintf "Content-Type: %s\r\n" content_type |> Lwt_bytes.of_string
  in
  IO_vector.append_bigarray iov content_type_header 0
    (Lwt_bytes.length content_type_header) ;
  let content_length = Lwt_bytes.length body in
  let content_length_header =
    Format.sprintf "Content-Length: %d\r\n" content_length
    |> Lwt_bytes.of_string in
  IO_vector.append_bigarray iov content_length_header 0
    (Lwt_bytes.length content_length_header) ;
  IO_vector.append_bytes iov (Bytes.unsafe_of_string "\r\n") 0 2 ;
  IO_vector.append_bigarray iov body 0 content_length ;
  Lwt_unix.writev conn iov >|= fun _ -> ()

let rec handle_requests request_handler client_addr fd =
  _debug (fun k -> k "Waiting for new request ...\n%!") ;
  Request.t client_addr fd
  >>= function
  | Ok req -> (
      _debug (fun k -> k "%s\n%!" (Request.show req)) ;
      request_handler ~conn:fd req
      >>= fun () ->
      List.assoc_opt "Connection" (Request.headers req)
      |> function
      | Some "close" -> Lwt_unix.close fd
      | Some _ | None -> handle_requests request_handler client_addr fd )
  | Error e ->
      _debug (fun k -> k "Error: %s" e) ;
      let response_txt = "400 Bad Request" in
      String.length response_txt
      |> Lwt_unix.write_string fd response_txt 0
      >|= fun _ -> ()

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
