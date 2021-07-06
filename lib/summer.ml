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

type error = string

(* module Parser = Reparse_lwt_unix.Fd *)
(* open Parser *)

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
end

module Option = struct
  include Option

  let ( >>= ) b f = Option.bind b f [@@warning "-32"]
end

module C = struct
  let transfer_encoding = "Transfer-Encoding"
  let trailer = "Trailer"
  let content_length = "Content-Length"
  let chunked = "chunked"
  let accept_encodings = "Accept-Encoding"
  let content_encodings = "Content-Encoding" [@@warning "-32"]
end

type encoding = {encoder: encoder; weight: float option}

and encoder =
  [`Compress | `Deflate | `Gzip | `Br | `Any | `None | `Other of string]

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

  let accept_encoding t =
    let open Reparse.String in
    let open Make_common (Reparse.String) in
    let weight =
      let qvalue1 =
        char '0' *> optional (char '.' *> take ~up_to:3 digit)
        >>= function
        | Some l -> string_of_chars ('0' :: '.' :: l) >>| float_of_string
        | None -> return 0. in
      let qvalue2 =
        char '1' *> optional (char '.' *> take ~up_to:3 (char '0'))
        >>= function
        | Some l -> string_of_chars ('1' :: '.' :: l) >>| float_of_string
        | None -> return 1. in
      let qvalue = qvalue1 <|> qvalue2 in
      ows *> char ';' *> ows *> string_ci "q=" *> qvalue in
    let coding = function
      | "compress" | "x-compress" -> return `Compress
      | "deflate" -> return `Deflate
      | "gzip" | "x-gzip" -> return `Gzip
      | "*" -> return `Any
      | "" -> return `None
      | enc -> return (`Other enc) in
    let p =
      let content_coding = token in
      let codings =
        content_coding <|> string_ci "identity" <|> string_cs "*" >>= coding
      in
      take
        ((codings, optional weight) <$$> fun encoder weight -> {encoder; weight})
    in
    match List.assoc_opt C.accept_encodings t.headers with
    | Some enc ->
        if String.(trim enc |> length) = 0 then
          Ok [{encoder= `None; weight= None}]
        else Reparse.String.(parse (create_input_from_string enc) p)
    | None -> Ok []

  (* let content_encoding t = [] *)

  open Reparse_lwt_unix.Fd
  open Make_common (Reparse_lwt_unix.Fd)

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
      parse input request_meta
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

let rec pp_encoding fmt t =
  let fields =
    [ Fmt.field "name" (fun p -> p.encoder) pp_encoder
    ; Fmt.field "weight" (fun p -> p.weight) Fmt.(option float) ] in
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

(**-- Processing body --*)

let is_chunked req =
  List.assoc_opt C.transfer_encoding (Request.headers req)
  |> function
  | Some encoding ->
      String.split_on_char ',' encoding
      |> List.map String.trim
      |> fun encodings -> if List.mem C.chunked encodings then true else false
  | None -> false

let read_body_chunks ~conn (Request.{headers; _} as req) ~on_chunk =
  let open Reparse_lwt_unix.Fd in
  let open Make_common (Reparse_lwt_unix.Fd) in
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
        |> List.filter (fun e -> not (String.equal e C.chunked))
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
  if is_chunked req then
    let input = Reparse_lwt_unix.Fd.create_input conn in
    chunked_body headers ~on_chunk |> parse input
  else Lwt_result.fail "[read_body_chunks] Not a `Chunked request body"

let deflate_decode str =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let w = De.make_window ~bits:15 in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let refill buf =
    let len = min (Bigstringaf.length str - !p) De.io_buffer_size in
    Bigstringaf.blit str ~src_off:!p buf ~dst_off:0 ~len ;
    p := !p + len ;
    len in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str in
  match De.Higher.uncompress ~w ~refill ~flush i o with
  | Ok () -> Ok (Buffer.contents r)
  | Error (`Msg s) -> Error s

let deflate_encode str =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let w = De.Lz77.make_window ~bits:15 in
  let q = De.Queue.create 0x1000 in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let refill buf =
    let len = min (Bigstringaf.length str - !p) De.io_buffer_size in
    Bigstringaf.blit str ~src_off:!p buf ~dst_off:0 ~len ;
    p := !p + len ;
    len in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str in
  De.Higher.compress ~w ~q ~refill ~flush i o ;
  Buffer.contents r

let gzip_decode (str : Bigstringaf.t) =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let refill buf =
    let len = min (Bigstringaf.length str - !p) De.io_buffer_size in
    Bigstringaf.blit str ~src_off:!p buf ~dst_off:0 ~len ;
    p := !p + len ;
    len in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str in
  match Gz.Higher.uncompress ~refill ~flush i o with
  | Ok (_ : Gz.Higher.metadata) -> Ok (Buffer.contents r)
  | Error (`Msg err) -> Error err

let time () = Int32.of_float (Unix.gettimeofday ())

let gzip_encode ?(level = 4) (str : Bigstringaf.t) =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let w = De.Lz77.make_window ~bits:15 in
  let q = De.Queue.create 0x1000 in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let cfg = Gz.Higher.configuration Gz.Unix time in
  let refill buf =
    let len = min (Bigstringaf.length str - !p) De.io_buffer_size in
    Bigstringaf.blit str ~src_off:!p buf ~dst_off:0 ~len ;
    p := !p + len ;
    len in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str in
  Gz.Higher.compress ~w ~q ~level ~refill ~flush () cfg i o ;
  Buffer.contents r

let supported_encodings : encoding list =
  [{encoder= `Gzip; weight= Some 1.0}; {encoder= `Deflate; weight= Some 0.0}]

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
          Format.sprintf
            "[read_body_content] Invalid 'Content-Length' header value: %s" len
          |> Lwt_result.fail) )
  | None ->
      Lwt_result.fail "[read_body_content] 'Content-Length' header not found"

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

let write_status conn status_code reason_phrase =
  let iov = IO_vector.create () in
  let status_line =
    Format.sprintf "HTTP/1.1 %d %s\r\n" status_code reason_phrase
    |> Lwt_bytes.of_string in
  IO_vector.append_bigarray iov status_line 0 (Lwt_bytes.length status_line) ;
  Lwt_unix.writev conn iov >|= fun _ -> ()

let rec handle_requests request_handler client_addr conn =
  _debug (fun k -> k "Waiting for new request ...\n%!") ;
  Request.t client_addr conn
  >>= function
  | Ok req -> (
      _debug (fun k -> k "%s\n%!" (Request.show req)) ;
      Lwt.catch
        (fun () -> request_handler ~conn req)
        (fun exn ->
          _debug (fun k -> k "Unhandled exception: %s" (Printexc.to_string exn)) ;
          write_status conn 500 "Internal Server Error" )
      >>= fun () ->
      List.assoc_opt "Connection" (Request.headers req)
      |> function
      | Some "close" -> Lwt_unix.close conn
      | Some _ | None -> handle_requests request_handler client_addr conn )
  | Error e ->
      _debug (fun k -> k "Error: %s" e) ;
      write_status conn 400 "Bad Request"

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
