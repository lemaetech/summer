let () =
  let port = ref 3000 in
  Arg.parse
    [("-p", Arg.Set_int port, " Listening port number (3000 by default)")]
    ignore "A echo HTTP server using summer" ;
  Summer.start !port (fun _addr _fd -> Lwt.return ())
