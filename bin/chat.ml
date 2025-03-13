open Lwt
open Lwt_io
open Lwt_unix
open Stdlib
open Mtime
open Mtime_clock

let (let*) = Lwt.bind

type message = Message | Receipt (* msg format: [type (1 byte)][timestamp (8 bytes)][msg length (4 bytes)][msg (N bytes)] *)
let mode = ref ""

let usage = 
  let program_name = Sys.argv.(0) in 
    Printf.sprintf "Usage: ./%s --server [PORT] OR ./%s --client HOST [PORT]\n" program_name program_name 

let get_time() = 
  Mtime.to_uint64_ns (Mtime_clock.now ())

let interpret_message channel = 
  let rec read input_channel buffer pos len = 
    if len <= 0 then Lwt.return true else
    let* bytes_read = Lwt_io.read_into input_channel buffer pos len in 
    if bytes_read = 0 then Lwt.return false else 
    read input_channel buffer (pos + bytes_read) (len - bytes_read)
  in
  let header = Bytes.create 13 in 
  let* result = read channel header 0 13 in 
  if not result then Lwt.return_none else 
  let msg_type = match Bytes.get header 0 with 
    | '\x01' -> Message 
    | '\x02' -> Receipt
    | _ -> failwith "Invalid message type."
  in 
  let sent_time = Bytes.get_int64_be header 1 in
  let msg_len = Bytes.get_int32_be header 9 |> Int32.to_int in
  let msg = Bytes.create msg_len in 
  let* result = read channel msg 0 msg_len in 
  if not result then Lwt.return_none else
  Lwt.return_some (msg_type, sent_time, msg)

let write_message output_channel msg_type msg = 
  let msg = (Bytes.of_string msg) in 
  let msg_len = Bytes.length msg in 
  let header = Bytes.create 13 in 
  let type_char = match msg_type with 
    | Message -> '\x01'
    | Receipt -> '\x02'
  in
  Bytes.set header 0 type_char;
  Bytes.set_int64_be header 1 (get_time());
  Bytes.set_int32_be header 9 (Int32.of_int msg_len);

  let* () = Lwt_io.write_from_exactly output_channel header 0 13 in 
  let* () = Lwt_io.write_from_exactly output_channel msg 0 msg_len in 
  Lwt_io.flush output_channel

let stdin_stream  = 
  let stream, push = Lwt_stream.create () in 
  Lwt.async ( fun () -> 
    let rec loop () = 
      let* line = Lwt_io.read_line Lwt_io.stdin in 
      push (Some line);
      loop ()
    in loop ());
    stream 

let shutdown_stream = Lwt_stream.clone stdin_stream
let send_stream = Lwt_stream.clone stdin_stream

let rec check_exit() = 
  let* line = Lwt_stream.next shutdown_stream in 
    if String.lowercase_ascii line = "exit" then 
      exit 0
  else 
    check_exit()

let converse socket = 
  let input = Lwt_io.of_fd ~mode:Lwt_io.input socket in 
  let output = Lwt_io.of_fd ~mode:Lwt_io.output socket in

  let poll = 
    let rec loop() = 
      let* msg = interpret_message input in 
      match msg with 
      | None -> Lwt.return_unit
      | Some (msg_type, sent_time, contents) -> 
        let received_time = get_time() in
        let sender = if (!mode = "SERVER") then "client" else "server" in 
        let contents = (Bytes.to_string contents) in 
        match msg_type with 
        | Message -> 
          let trip_time = Int64.sub received_time sent_time in 
          let* () = write_message output Receipt (Int64.to_string trip_time) in 
          let* () = Lwt_io.printlf "%s: %s" (String.capitalize_ascii sender) contents in 
          loop()
        | Receipt -> 
          let* () = Lwt_io.printlf "✅ Received by %s! - %s ns" sender contents in
          loop()
    in loop()
  in 
  let send = 
    let rec loop() = 
      let* msg = Lwt_stream.next send_stream in 
      if (String.lowercase_ascii msg) = "exit" then 
        let* () = write_message output Message "Exiting..." in
        let* () = Lwt_io.flush output in
        exit 0;
    else
      let* () = write_message output Message msg in
      loop()
    in loop() 
  in 
  Lwt.catch
  (fun () -> Lwt.pick [send; poll])
  ( fun exc ->
    match exc with
    | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
      let* () = Lwt_unix.close socket in
      Lwt.return_unit
    | exc ->
      let* () = Lwt_unix.close socket in
      Lwt.fail exc)

let validated_address address =
  match (try Some (Unix.inet_addr_of_string address) with _ -> None) with
  | Some inet_addr -> inet_addr
  | None -> 
    match (try Some (Unix.(gethostbyname address).h_addr_list.(0)) with _ -> None) with
    | None -> 
      Printf.printf "Could not establish host.";
      exit 1
    | Some host -> host

let connect socket address =
  Lwt.catch
    (fun () -> Lwt_unix.connect socket address)
    (fun exc ->
      let* () = Lwt_io.printf "Connection error: %s\n" (Printexc.to_string exc) in
      Lwt.fail exc) 
        
let rec listen socket =
  let handle_client (client_socket, _) =
    let* () = Lwt_io.printl "Client joined the server!" in
    let output = Lwt_io.of_fd ~mode:Lwt_io.Output client_socket in
    let* () = write_message output Message "Welcome to the server!" in
    Lwt.async (fun () -> converse client_socket);
    listen socket 
  in 
  let* result =
    Lwt.catch
      (fun () ->
        let* client = Lwt_unix.accept socket in
        Lwt.return_some client)
      (fun exc ->
        let* () = Lwt_io.eprintlf "Error accepting connection: %s" (Printexc.to_string exc) in
        Lwt.return_none)
  in
  match result with
  | Some client -> handle_client client
  | None -> Lwt.return_unit

let () =
  let argc = Array.length Sys.argv in
  if argc < 2 then
    Printf.printf "%s" usage
  else
    mode := (String.sub (String.uppercase_ascii Sys.argv.(1)) 2 6);
    match !mode with
    | m when m = "CLIENT" || m = "SERVER" ->
        let default_port = "8080" in          
        let default_host = "127.0.0.1" in
        let (host, port) = 
          match m, argc with
          | _,2 -> (default_host, default_port)  (* server with default port OR client with default host and default port*)
          | "SERVER", 3 -> (default_host, Sys.argv.(2))  (* server with specific port *)
          | "CLIENT", 3 -> (Sys.argv.(2), default_port)  (* client with specific host, default port *)
          | "CLIENT", 4 -> (Sys.argv.(2), Sys.argv.(3))  (* client with specific host and port *)
          | _,_ -> 
            Printf.printf "%s" usage;
            exit 1
          in
          let port = int_of_string port in
          let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
          Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
          Lwt_unix.setsockopt socket Unix.TCP_NODELAY true; 
          let address = if ((!mode) = "SERVER") then Unix.inet_addr_any else (validated_address host) in
          let sockaddr = Unix.ADDR_INET (address, port) in
          Lwt_main.run (
          match m with
          | "SERVER" -> 
            let* () = Lwt_unix.bind socket sockaddr in 
            Lwt_unix.listen socket 10;
            let* () = Lwt_io.printlf "Server running! (Port: %d)" port in  
            Lwt.pick[check_exit();listen socket]
          | "CLIENT" -> 
            let* () = connect socket sockaddr in
            converse socket
          | _ -> failwith "bad mode passed in"
          )
  | _ ->
    Printf.printf "%s" usage