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

and body_type =
  [ `Chunked
  | `Content of content_length
  | `Multipart of content_length * boundary
  | `None ]

and content_length = int

and boundary = Http_multipart_formdata.boundary

and chunk_body = {data: Cstruct.t; chunk_extensions: chunk_extension list}

and chunk_extension = {name: string; value: string option}

and encoding = {encoder: encoder; weight: float option}

and encoder =
  [`Compress | `Deflate | `Gzip | `Br | `Any | `None | `Other of string]

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
  let transfer_encoding = "transfer-encoding"
  let trailer = "trailer"
  let content_length = "content-length"
  let chunked = "chunked"
  let accept_encoding = "accept-encoding"
  let content_encoding = "content-encoding" [@@warning "-32"]
  let content_type = "content-type" [@@warning "-32"]
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

let coding = function
  | "compress" | "x-compress" -> `Compress
  | "deflate" -> `Deflate
  | "gzip" | "x-gzip" -> `Gzip
  | "*" -> `Any
  | "" -> `None
  | enc -> `Other enc

let content_length request =
  match Hashtbl.find_opt request.headers C.content_length with
  | Some content_length -> (
    try Ok (int_of_string content_length)
    with _ ->
      Error
        (Format.sprintf "Invalid '%s' value: %s" C.content_length content_length)
    )
  | None -> Error (Format.sprintf "%s header not found" C.content_length)

let accept_encoding t =
  let open Reparse.String in
  let open Make_common (Reparse.String) in
  let weight =
    let qvalue1 =
      char '0' *> optional (char '.' *> take ~up_to:3 digit)
      >>= function
      | Some l -> string_of_chars ('0' :: '.' :: l) >>| float_of_string
      | None -> return 0.
    in
    let qvalue2 =
      char '1' *> optional (char '.' *> take ~up_to:3 (char '0'))
      >>= function
      | Some l -> string_of_chars ('1' :: '.' :: l) >>| float_of_string
      | None -> return 1.
    in
    let qvalue = qvalue1 <|> qvalue2 in
    ows *> char ';' *> ows *> string_ci "q=" *> qvalue
  in
  let accept_encoding_parser =
    let content_coding = token in
    let codings =
      content_coding <|> string_ci "identity" <|> string_cs "*" >>| coding
    in
    take
      ((codings, optional weight) <$$> fun encoder weight -> {encoder; weight})
  in
  match Hashtbl.find_opt t.headers C.accept_encoding with
  | Some enc ->
      if String.(trim enc |> length) = 0 then Ok [{encoder= `None; weight= None}]
      else
        Reparse.String.(
          parse (create_input_from_string enc) accept_encoding_parser)
        |> Result.map (fun (x, _) -> x)
  | None -> Ok []

let content_encoding t =
  match Hashtbl.find_opt t.headers C.accept_encoding with
  | Some enc ->
      String.split_on_char ',' enc
      |> List.map (fun enc -> String.trim enc |> coding)
  | None -> []

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

let rec pp_encoding fmt t =
  let fields =
    [ Fmt.field "name" (fun p -> p.encoder) pp_encoder
    ; Fmt.field "weight" (fun p -> p.weight) Fmt.(option float) ]
  in
  Fmt.record fields fmt t

and pp_encoder fmt coding =
  ( match coding with
  | `Compress -> "compress"
  | `Deflate -> "deflate"
  | `Gzip -> "gzip"
  | `Br -> "br"
  | `Any -> "*"
  | `None -> ""
  | `Other enc -> Format.sprintf "Other (%s)" enc )
  |> Format.fprintf fmt "%s"

let request ctx = ctx.request
let conn ctx = ctx.conn

(**-- Processing body --*)

(** Determine request body type in the order given below,

    - If [Transfer-Encoding: chunked] is present then [`Chunked]
    - If [Content-Length: len] is present and [Content-Type: multipart/formdata]
      is present then [`Multipart boundary] else [`Content len]
    - Else [`None] *)
let rec body_type request =
  match chunked_body request with
  | Ok `None -> content_body request
  | Ok `Chunked as ok -> ok
  | Error _ as err -> err

and chunked_body req =
  match Hashtbl.find_opt req.headers C.transfer_encoding with
  | Some te -> (
      let codings =
        List.(String.split_on_char ',' te |> map String.trim |> rev)
      in
      match List.hd codings with
      | exception _ ->
          Error
            (Format.sprintf
               "Invalid header '%s'. You need to specify at least one coding. \
                Please see \
                https://datatracker.ietf.org/doc/html/rfc7230#section-3.3.1"
               C.transfer_encoding )
      | coding when String.equal coding C.chunked -> Result.ok `Chunked
      | _ -> Result.ok `None )
  | None -> Result.ok `None

and content_body req =
  match Hashtbl.find_opt req.headers C.content_length with
  | Some content_length -> (
    try
      let len = int_of_string content_length in
      match multipart_body req with
      | Ok `None -> Ok (`Content len)
      | Ok (`Boundary boundary) -> Ok (`Multipart (len, boundary))
      | Error _ as err -> err
    with _ ->
      Error
        (Format.sprintf "Invalid '%s' value: %s" C.content_length content_length)
    )
  | None -> Ok `None

and multipart_body req =
  match Hashtbl.find_opt req.headers C.content_type with
  | None -> Ok `None
  | Some content_type -> (
    match Http_multipart_formdata.parse_boundary ~content_type with
    | Ok boundary -> Ok (`Boundary boundary)
    | Error _ -> Ok `None )

let body_reader context =
  let input = Reparse_lwt_unix.Fd.create_input context.conn in
  {input; pos= 0; total_read= 0}

let read_chunked reader context =
  let chunk_parser reader =
    let open Reparse_lwt_unix.Fd in
    let open Make_common (Reparse_lwt_unix.Fd) in
    let quoted_pair = char '\\' *> (whitespace <|> vchar) in
    (*-- qdtext = HTAB / SP /%x21 / %x23-5B / %x5D-7E / obs-text -- *)
    let qdtext =
      htab <|> space <|> char '\x21'
      <|> char_if (function
            | '\x23' .. '\x5B' -> true
            | '\x5D' .. '\x7E' -> true
            | _ -> false )
    in
    (*-- quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE --*)
    let quoted_string =
      take_between ~start:(char '"')
        (take (qdtext <|> quoted_pair) >>= string_of_chars)
        ~end_:(char '"')
      >>| String.concat ""
    in
    (*-- https://datatracker.ietf.org/doc/html/rfc7230#section-4.1 --*)
    let chunk_ext =
      let chunk_ext_name = token in
      let chunk_ext_val = quoted_string <|> token in
      take
        ( (char ';' *> chunk_ext_name, optional (char '=' *> chunk_ext_val))
        <$$> fun name value : chunk_extension -> {name; value} )
      <?> "[chunk_ext]"
    in
    let chunk_size =
      take ~at_least:1 hex_digit >>= string_of_chars
      >>= fun sz ->
      try return (Format.sprintf "0x%s" sz |> int_of_string)
      with _ -> fail (Format.sprintf "[chunk_size] Invalid chunk_size: %s" sz)
    in
    (* Chunk decoding algorithm is explained at
       https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.3 *)
    let* size, chunk_extensions = (chunk_size, chunk_ext <* crlf) <$$> pair in
    if size > 0 then (
      let+ data = unsafe_take_cstruct size <* trim_input_buffer in
      reader.total_read <- reader.total_read + size ;
      `Chunk {data; chunk_extensions} )
    else if size = 0 then (
      (* If chunk size is 0 then read the chunk trailers and update the context
         request.

         The spec at https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.3
         specifies that 'Content-Length' and 'Transfer-Encoding' headers must be
         updated. *)
      let request_headers = context.request.headers in
      (* Be strict about headers allowed in trailer headers to minimize security
         issues, eg. request smuggling attack -
         https://portswigger.net/web-security/request-smuggling

         Allowed headers are defined in 2nd paragraph of
         https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.2 *)
      let is_trailer_header_allowed = function
        | "transfer-encoding" | "content-length" | "host"
        (* Request control headers are not allowed. *)
         |"cache-control" | "expect" | "max-forwards" | "pragma" | "range"
         |"te"
        (* Authentication headers are not allowed. *)
         |"www-authenticate" | "authorization" | "proxy-authenticate"
         |"proxy-authorization"
        (* Cookie headers are not allowed. *)
         |"cookie" | "set-cookie"
        (* Response control data headers are not allowed. *)
         |"age" | "expires" | "date" | "location" | "retry-after" | "vary"
         |"warning"
        (* Headers to process the payload are not allowed. *)
         |"content-encoding" | "content-type" | "content-range" | "trailer" ->
            false
        | _ -> true
      in
      let+ trailer_headers =
        let trailer_specified_headers =
          Hashtbl.find_opt request_headers C.trailer
          |> function
          | Some v -> String.split_on_char ',' v |> List.map String.trim
          | None -> []
        in
        let+ headers = header_fields <* crlf <* trim_input_buffer in
        List.filter
          (fun (name, _) ->
            List.mem name trailer_specified_headers
            && is_trailer_header_allowed name )
          headers
      in
      List.iter
        (fun (key, value) -> Hashtbl.replace request_headers key value)
        trailer_headers ;
      (* Remove either just the 'chunked' from Transfer-Encoding header value or
         remove the header entirely. *)
      Hashtbl.find request_headers C.transfer_encoding
      |> String.split_on_char ',' |> List.map String.trim
      |> List.filter (fun e -> not (String.equal e C.chunked))
      |> String.concat ","
      |> fun te ->
      if String.length te > 0 then
        Hashtbl.replace request_headers C.transfer_encoding te
      else Hashtbl.remove request_headers C.transfer_encoding ;
      (* Remove Trailer header. *)
      Hashtbl.remove request_headers C.trailer ;
      (* Add/Update Content-Length header. *)
      Hashtbl.replace request_headers C.content_length
        (string_of_int reader.total_read) ;
      `End )
    else fail (Format.sprintf "Invalid chunk size: %d" size)
  in
  Lwt.(
    Reparse_lwt_unix.Fd.parse ~pos:reader.pos reader.input (chunk_parser reader)
    >>= function
    | Ok (x, pos) ->
        reader.pos <- pos ;
        return x
    | Error e -> return (`Error e))

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
