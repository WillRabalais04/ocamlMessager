open Lwt
open Lwt_io
open Lwt_unix
open Stdlib
open Mtime
open Mtime_clock

let (let*) = Lwt.bind

type message = Message | Receipt (* msg format: [type (1 byte)][timestamp (8 bytes)][msg length (4 bytes)][msg (N bytes)] *)
let mode = ref ""
let connected = ref false

let create_socket () =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.setsockopt socket Unix.SO_KEEPALIVE true;
  socket

let usage = 
  let program_name = Sys.argv.(0) in 
    Printf.sprintf "Usage: ./%s --server [PORT] OR ./%s --client HOST [PORT]\n" program_name program_name 

let message_byte = function
  | Message -> '\x01'
  | Receipt -> '\x02'

let byte_to_message = function
  | '\x01' -> Message
  | '\x02' -> Receipt
  | _ -> failwith "Invalid message type."

let run_with_error_handling err_name f  =
    Lwt.catch f
    (fun exn ->
      let* () = Lwt_io.eprintlf "Error in %s: %s" err_name (Printexc.to_string exn) in
      match exn with
      | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
        let* () = Lwt_io.printlf "Connection closed." in 
        Lwt.return_unit
      | _ -> Lwt.return_unit)

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
  let msg_type =  byte_to_message (Bytes.get header 0) in 
  let sent_time = Bytes.get_int64_be header 1 in
  let msg_len = Bytes.get_int32_be header 9 |> Int32.to_int in
  let msg = Bytes.create msg_len in 
  let* result = read channel msg 0 msg_len in 
  if not result then Lwt.return_none else
  Lwt.return_some (msg_type, sent_time, msg)

let write_message output_channel msg_type msg = 
  let msg = Bytes.of_string msg in
  let msg_type = (message_byte msg_type) in
  let send_time = Mtime_clock.now_ns () in
  let msg_len = Bytes.length msg in 
  
  let header = Bytes.create 13 in 
  Bytes.set header 0 msg_type; 
  Bytes.set_int64_be header 1 send_time; 
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

let message_channel = Lwt_mvar.create_empty ()

let rec stdin_handler () = 
  let* line = Lwt_stream.next stdin_stream in 
  match (String.lowercase_ascii line) with 
  | "/exit" -> 
    exit 0;
  | _ ->
    if !mode = "--SERVER" && not !connected then 
      let* () = Lwt_io.printlf "Command unknown. No active client connection." in 
      stdin_handler()
    else 
      let* () = Lwt_mvar.put message_channel line in 
      stdin_handler()

let connect socket address =
  let connection_attempt = Lwt_unix.connect socket address in
  let timeout_promise = Lwt_unix.sleep 5.0 >>= fun () -> Lwt.fail (Failure "Connection timed out") in
  Lwt.pick [connection_attempt; timeout_promise]

let converse socket  = 
  let input = Lwt_io.of_fd ~mode:Lwt_io.input socket in 
  let output = Lwt_io.of_fd ~mode:Lwt_io.output socket in

  let poll = 
    let rec loop() = 
      let* msg = interpret_message input in 
      match msg with 
      | None ->
        let* () = Lwt_io.printlf "Connection closed." in
        connected := false;
        Lwt.return_unit
      | Some (msg_type, sent_time, contents) -> 
        let received_time = Mtime_clock.now_ns () in
        let sender = if (!mode = "--SERVER") then "client" else "server" in 
        match msg_type with 
        | Message -> 
          let* () = write_message output Receipt (Int64.to_string sent_time) in 
          let* () = Lwt_io.printlf "%s: %s" (String.capitalize_ascii sender) (Bytes.to_string contents) in 
          loop()
        | Receipt -> 
          let sent_time = Int64.of_string (Bytes.to_string contents) in
          let rtt = Int64.sub received_time sent_time in 
          let* () = Lwt_io.printlf "✅ Received by %s! - %Ld ns" sender rtt in
          loop()
    in loop()
  in 
  let send = 
    let rec loop() = 
      let* msg = Lwt_mvar.take message_channel in
      let* () =  write_message output Message msg in
      loop()
    in loop()
  in 
  run_with_error_handling "conversing" (fun () -> Lwt.pick [send; poll])

let validated_address address =
  try
    let inet_addr = Unix.inet_addr_of_string address in
    inet_addr
  with
  | Unix.Unix_error (Unix.EINVAL, _, _) ->
      (try
        let host = Unix.(gethostbyname address).h_addr_list.(0) in
        host
      with
      | Unix.Unix_error (Unix.EHOSTUNREACH, _, _) -> 
          Printf.printf "Could not resolve host.\n";
          exit 1
      | _ -> 
          Printf.printf "Could not resolve host: %s.\n" address;
          exit 1)
  | _ -> 
      Printf.printf "Invalid IP address format: %s.\n" address;
      exit 1

let handle_client client =     
  let output = Lwt_io.of_fd ~mode:Lwt_io.Output client in
  let* () = Lwt_stream.junk_old stdin_stream in
  let* () = Lwt_io.printl "Client joined the server!\nSending welcome message..." in
  let* () = write_message output Message "Welcome to the server!" in
  let* () = run_with_error_handling "client handling" (fun () -> converse client) in
  connected := false;
  let* () = Lwt_unix.close client in
  Lwt.return_unit

let rec server_loop sockaddr =
  let server_socket = create_socket () in
  let* () = Lwt_unix.bind server_socket sockaddr in
  Lwt_unix.listen server_socket 1;
  let* () = Lwt_io.printlf "Listening for a connection..." in
  
  let* result =
    Lwt.catch
      (fun () ->
        let* (client, _) = Lwt_unix.accept server_socket in
        connected := true;
        Lwt.return_some client)
      (fun exn ->
        let* () = Lwt_io.eprintlf "Error in server accept: %s" (Printexc.to_string exn) in
        Lwt.return_none)
  in
  let* () = Lwt_unix.close server_socket in
  match result with
  | Some client -> 
      let* () = handle_client client in
      server_loop sockaddr
  | None -> 
      let* () = Lwt_unix.sleep 1.0 in
      server_loop sockaddr

let () =
  let argc = Array.length Sys.argv in
  if argc < 2 then
    Printf.printf "%s" usage
  else
    mode := (String.uppercase_ascii Sys.argv.(1));
    match !mode with
    | m when m = "--CLIENT" || m = "--SERVER" ->
        let default_port = "8080" in          
        let default_host = "127.0.0.1" in
        let (host, port) = 
          match m, argc with
          | _,2 -> (default_host, default_port)  (* server with default port OR client with default host and default port*)
          | "--SERVER", 3 -> (default_host, Sys.argv.(2))  (* server with specific port *)
          | "--CLIENT", 3 -> (Sys.argv.(2), default_port)  (* client with specific host, default port *)
          | "--CLIENT", 4 -> (Sys.argv.(2), Sys.argv.(3))  (* client with specific host and port *)
          | _,_ -> 
            Printf.printf "%s" usage;
            exit 1
          in
          let port = int_of_string port in
          let address = if ((!mode) = "--SERVER") then Unix.inet_addr_any else (validated_address host) in
          let sockaddr = Unix.ADDR_INET (address, port) in
          Lwt_main.run (
          match m with
          | "--SERVER" -> 
            let* () = Lwt_io.printlf "Server running! (Port: %d)" port in  
            Lwt.join[stdin_handler(); server_loop sockaddr]
          | "--CLIENT" -> 
            let socket = create_socket () in 
            let* () = run_with_error_handling "connecting" (fun () -> 
            let* () = Lwt_io.printlf "Connecting to %s:%d..." host port in
            connect socket sockaddr) in
            connected := true;
            let* () = Lwt.pick[stdin_handler(); converse socket] in 
            Lwt.return_unit
          | _ -> failwith "bad mode passed in"
          )
  | _ ->
    Printf.printf "%s" usage