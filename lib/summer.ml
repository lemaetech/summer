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

module Smap = Map.Make (String)

type request =
  { method': method'
  ; target: string
  ; http_version: int * int
  ; content_length: int option
  ; headers: (string * string) list
  ; client_addr: string (* IP address of client. *)
  ; fd: Lwt_unix.file_descr
  ; anticsrf_token: string
  ; cookies: Http_cookie.t list
  ; session_data: session_data
  ; mutable unconsumed: Cstruct.t
        (* unconsumed - bytes remaining after request is processed *)
  ; mutable body_read: bool
        (* body_read - denotes if the request body has been read or not. This is used
             to determine if the connection socket needs to be drained before
             reading another request in the same connection. *)
  ; mutable multipart_reader: Http_multipart_formdata.reader option }

(* https://datatracker.ietf.org/doc/html/rfc7231#section-4 *)
and method' = Wtr.method'

and header = string * string
(* (name,value) *)

and session_data = (string, string) Hashtbl.t

and session_id = string

and memory_storage = (session_id, session_data) Hashtbl.t

type response =
  { response_code: response_code
  ; headers: string Smap.t
  ; cookies: Http_cookie.t Smap.t
  ; body: Cstruct.t }

and response_code = int * string
(* code, reason phrase *)

and handler = request -> response Lwt.t

and middleware = handler -> handler

and key = Cstruct.t

exception Request_error of string

let io_buffer_size = 65536 (* UNIX_BUFFER_SIZE 4.0.0 *)

let method' request = request.method'
let target request = request.target
let http_version request = request.http_version
let headers (request : request) = request.headers
let client_addr request = request.client_addr
let content_length request = request.content_length

let cookies (request : request) =
  request.cookies |> List.map (fun cookie -> (Http_cookie.name cookie, cookie))

let rec pp_request fmt (t : request) =
  let fields =
    [ Fmt.field "meth" (fun p -> p.method') Wtr.pp_method
    ; Fmt.field "target" (fun p -> p.target) Fmt.string
    ; Fmt.field "http_version" (fun p -> p.http_version) pp_http_version
    ; Fmt.field "headers" (fun (p : request) -> p.headers) pp_headers ]
  in
  Fmt.record fields fmt t

and pp_http_version fmt t =
  let comma' fmt _ = Fmt.string fmt "," in
  Fmt.(pair ~sep:comma' int int) fmt t

and pp_headers fmt (t : header list) =
  let colon fmt _ = Fmt.string fmt ": " in
  let header_field = Fmt.(pair ~sep:colon string string) in
  Fmt.vbox Fmt.(list header_field) fmt t

let request_to_string t =
  let buf = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buf in
  pp_request fmt t ; Format.fprintf fmt "%!" ; Buffer.contents buf

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
  let* meth = token >>| Wtr.method' <* space in
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
         | Some len -> begin
           try return (Some (int_of_string len))
           with _ ->
             raise
               (Request_error
                  (Format.sprintf "Invalid content-length value: %s" len) )
         end
         | None -> return None
       in
       `Request_line
         (meth, request_target, http_version, content_length, headers) )
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
    | `Request_line (method', target, http_version, content_length, headers)
      -> (
        let request =
          { method'
          ; target
          ; http_version
          ; content_length
          ; headers
          ; cookies= []
          ; anticsrf_token= ""
          ; session_data= Hashtbl.create 0
          ; client_addr= socketaddr_to_string client_addr
          ; fd
          ; body_read= false
          ; unconsumed
          ; multipart_reader= None }
        in
        match List.assoc_opt "cookie" headers with
        | Some v -> begin
          match Http_cookie.of_cookie v with
          | Ok cookies -> `Request {request with cookies}
          | Error e -> `Error e
        end
        | None -> `Request request )
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
let form_multipart ?(body_buffer_size = io_buffer_size) request =
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

let form_multipart_all request =
  let rec read_parts parts =
    form_multipart request
    >>= function
    | `End -> Lwt.return (List.rev parts)
    | `Header header ->
        let* body = read_body Cstruct.empty in
        read_parts ((header, body) :: parts)
    | `Error e -> raise (Request_error e)
    | _ -> assert false
  and read_body body =
    form_multipart request
    >>= function
    | `Body_end -> Lwt.return body
    | `Body buf -> read_body (Cstruct.append body buf)
    | `Error e -> raise (Request_error e)
    | _ -> assert false
  in
  read_parts []

let form_urlencoded (request : request) =
  match List.assoc_opt "content-type" request.headers with
  | Some "application/x-www-form-urlencoded" ->
      let+ body = body request in
      if body = "" then [] else Uri.query_of_encoded body
  | Some _ | None -> Lwt.return []

(* Response *)
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

let code_int : response_code -> int = fun (code, _) -> code
let code_reason_phrase (_, phrase) = phrase
let ok = response_code 200
let internal_server_error = response_code 500
let bad_request = response_code 400

let response_bigstring ?(response_code = ok) ?(headers = []) body =
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

let text body =
  response ~response_code:ok
    ~headers:[("content-type", "text/plain; charset=UTF-8")]
    body

let html body =
  response ~response_code:ok
    ~headers:[("content-type", "text/html; charset=UTF-8")]
    body

let tyxml doc =
  let buf = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "%a@." (Tyxml.Html.pp ~indent:true ()) doc ;
  html (Buffer.contents buf)

let not_found _ = Lwt.return @@ response ~response_code:(response_code 404) ""

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

(* Routing *)
let router router next_handler request =
  match Wtr.match' request.method' request.target router with
  | Some handler -> handler request
  | None -> next_handler request

(* Encryption/decryption *)

let initialize_rng = lazy (Mirage_crypto_rng_unix.initialize ())
let nonce_size = 12

let key sz =
  Lazy.force initialize_rng ;
  Mirage_crypto_rng.generate sz

external key_to_cstruct : key -> Cstruct.t = "%identity"

let key_to_base64 key =
  Cstruct.to_string key
  |> Base64.(encode_string ~pad:false ~alphabet:uri_safe_alphabet)

let key_of_base64 s =
  match Base64.(decode ~pad:false ~alphabet:uri_safe_alphabet s) with
  | Ok s -> Ok (Cstruct.of_string s)
  | Error (`Msg msg) -> Error msg

let encrypt_base64 key contents =
  assert (String.length contents > 0) ;
  let key = Mirage_crypto.Chacha20.of_secret key in
  let nonce = Mirage_crypto_rng.generate nonce_size in
  let encrypted =
    Mirage_crypto.Chacha20.authenticate_encrypt ~key ~nonce
      (Cstruct.of_string contents)
  in
  Cstruct.concat [nonce; encrypted]
  |> Cstruct.to_string
  |> Base64.(encode_string ~pad:false ~alphabet:uri_safe_alphabet)

let decrypt_base64 key contents =
  assert (String.length contents > 0) ;
  try
    let key = Mirage_crypto.Chacha20.of_secret key in
    let contents =
      Base64.(decode_exn ~pad:false ~alphabet:uri_safe_alphabet contents)
      |> Cstruct.of_string
    in
    let nonce = Cstruct.sub contents 0 nonce_size in
    Cstruct.sub contents nonce_size (Cstruct.length contents - nonce_size)
    |> Mirage_crypto.Chacha20.authenticate_decrypt ~key ~nonce
    |> function
    | Some s -> Cstruct.to_string s
    | None -> raise (Request_error "Unable to decrypt contents")
  with exn ->
    raise (Request_error (Format.sprintf "%s" (Printexc.to_string exn)))

(* Anti CSRF *)

let anticsrf_token request = request.anticsrf_token

let anticsrf ?(protect_http_methods = [`POST; `PUT; `DELETE]) ?excluded_routes
    key' next request =
  let cookie_name = "XSRF-TOKEN" in
  let anticsrf_tokname = "x-xsrf-token" in
  (* Implements double submit anti-csrf mechnism.

     https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html#double-submit-cookie
  *)
  let validate_anticsrf_token () =
    let method_protected =
      List.exists
        (fun method' -> method' = request.method')
        protect_http_methods
    in
    let route_excluded =
      Option.bind excluded_routes (fun router ->
          Wtr.match' request.method' request.target router )
      |> Option.value ~default:false
    in
    if method_protected && not route_excluded then
      let anticsrf_tok_cookie =
        match List.assoc_opt cookie_name (cookies request) with
        | Some c -> decrypt_base64 key' (Http_cookie.value c)
        | None ->
            raise
              (Request_error
                 (Format.sprintf "Anti-csrf cookie %s not found" cookie_name) )
      in
      let* anticsrf_tok =
        match List.assoc_opt anticsrf_tokname request.headers with
        | Some anticsrf_tok -> Lwt.return anticsrf_tok
        | None -> begin
            let+ form = form_urlencoded request in
            match List.assoc_opt anticsrf_tokname form with
            | Some [anticsrf_tok] -> anticsrf_tok
            | Some _ | None ->
                raise
                  (Request_error
                     (Format.sprintf "Anti-csrf token %s not found"
                        anticsrf_tokname ) )
          end
      in
      let anticsrf_tok = decrypt_base64 key' anticsrf_tok in
      if String.equal anticsrf_tok_cookie anticsrf_tok then Lwt.return request
      else raise (Request_error "Anti-csrf tokens donot match")
    else Lwt.return request
  in
  let* request = validate_anticsrf_token () in
  let anticsrf_token = key 32 |> Cstruct.to_string |> encrypt_base64 key' in
  let request = {request with anticsrf_token} in
  let+ response = next request in
  let anticsrf_cookie =
    Http_cookie.create ~http_only:false ~same_site:`Strict ~name:cookie_name
      anticsrf_token
    |> Result.get_ok
  in
  add_cookie anticsrf_cookie response

(* Session *)

let session_put request ~key value =
  Hashtbl.replace request.session_data key value

let session_find key request = Hashtbl.find_opt request.session_data key
let session_all request = Hashtbl.to_seq request.session_data |> List.of_seq
let memory_storage () = Hashtbl.create 0

let cookie_session ?expires ?max_age ?http_only ~cookie_name key next_handler
    request =
  let encode_session_data session_data =
    Hashtbl.to_seq session_data
    |> List.of_seq
    |> List.map (fun (key, value) -> Csexp.(List [Atom key; Atom value]))
    |> fun l -> Csexp.List l |> Csexp.to_string |> encrypt_base64 key
  in
  let decode_session_data session_data =
    let[@inline] err () = raise (Request_error "Invalid cookie session data") in
    let csexp = decrypt_base64 key session_data in
    let csexp =
      match Csexp.parse_string csexp with
      | Ok v -> v
      | Error (o, s) ->
          raise
            (Request_error
               (Format.sprintf "Csexp parsing error at offset '%d': %s" o s) )
    in
    match csexp with
    | Csexp.List key_values ->
        List.map
          (function
            | Csexp.(List [Atom key; Atom value]) -> (key, value) | _ -> err ()
            )
          key_values
        |> List.to_seq |> Hashtbl.of_seq
    | Csexp.Atom _ -> err ()
  in
  let request =
    match List.assoc_opt cookie_name (cookies request) with
    | Some cookie ->
        { request with
          session_data= decode_session_data (Http_cookie.value cookie) }
    | None -> {request with session_data= Hashtbl.create 0}
  in
  (* Add session cookie to response *)
  let open Lwt.Syntax in
  let+ response = next_handler request in
  let session_data = encode_session_data request.session_data in
  let cookie =
    Http_cookie.create ?expires ?max_age ?http_only ~same_site:`Strict
      ~name:cookie_name session_data
    |> Result.get_ok
  in
  add_cookie cookie response

let memory_session ?expires ?max_age ?http_only ~cookie_name ms next_handler
    request =
  let session_id, request =
    match List.assoc_opt cookie_name (cookies request) with
    | Some session_cookie ->
        let session_id = Http_cookie.value session_cookie in
        let session_data =
          Option.value
            (Hashtbl.find_opt ms session_id)
            ~default:(Hashtbl.create 0)
        in
        (session_id, {request with session_data})
    | None ->
        let session_id = key_to_base64 @@ key 32 in
        (session_id, {request with session_data= Hashtbl.create 0})
  in
  (* Add session cookie to response *)
  let open Lwt.Syntax in
  let+ response = next_handler request in
  Hashtbl.replace ms session_id request.session_data ;
  let cookie =
    Http_cookie.create ?expires ?max_age ?http_only ~same_site:`Strict
      ~name:cookie_name session_id
    |> Result.get_ok
  in
  add_cookie cookie response

(* Write response *)

module IO_vector = Lwt_unix.IO_vectors

let write_response fd {response_code; headers; body; cookies} =
  let iov = IO_vector.create () in

  (* Write response status line. *)
  let status_line =
    Format.sprintf "HTTP/1.1 %d %s\r\n" (code_int response_code)
      (code_reason_phrase response_code)
    |> Bytes.unsafe_of_string
  in
  IO_vector.append_bytes iov status_line 0 (Bytes.length status_line) ;

  let body_len = Cstruct.length body in

  (* Add set-cookie headers if we have cookies. *)
  let headers =
    if Smap.cardinal cookies > 0 then
      let headers =
        Smap.fold
          (fun _cookie_name cookie acc ->
            Smap.add "set-cookie" (Http_cookie.to_set_cookie cookie) acc )
          cookies headers
      in
      (* Update cache-control header so that we don't cache cookies. *)
      let no_cache = {|no-cache="Set-Cookie"|} in
      Smap.update "cache-control"
        (function
          | Some v -> Some (Format.sprintf "%s, %s" v no_cache)
          | None -> Some no_cache )
        headers
    else headers
  in

  (* Add content-length headers if it doesn't exist. *)
  let headers =
    if Smap.mem "content-length" headers then headers
    else Smap.add "content-length" (string_of_int body_len) headers
  in

  (* Write response headers. *)
  Smap.iter
    (fun name v ->
      let buf = Format.sprintf "%s: %s\r\n" name v |> Bytes.unsafe_of_string in
      IO_vector.append_bytes iov buf 0 (Bytes.length buf) )
    headers ;

  (* Write response body. *)
  IO_vector.append_bytes iov (Bytes.unsafe_of_string "\r\n") 0 2 ;
  if body_len > 0 then
    IO_vector.append_bigarray iov (Cstruct.to_bigarray body) 0 body_len ;

  Lwt_unix.writev fd iov >|= fun _ -> ()

(* Handle request*)
let rec handle_requests unconsumed handler client_addr fd =
  _debug (fun k -> k "Waiting for new request ...\n%!") ;
  request fd unconsumed client_addr
  >>= function
  | `Request req -> (
      _debug (fun k -> k "%s\n%!" (request_to_string req)) ;
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
              write_response fd (response ~response_code:bad_request "")
          | exn ->
              _debug (fun k -> k "Exception: %s" (Printexc.to_string exn)) ;
              write_response fd
                (response ~response_code:internal_server_error "") )
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
      write_response fd (response ~response_code:bad_request "")
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
