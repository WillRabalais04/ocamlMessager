open Lwt
open Lwt_io
open Lwt_unix
open Stdlib
open Mtime
open Mtime_clock

let (let*) = Lwt.bind

let usage_msg = 
  let program_name = Sys.argv.(0) in 
  Printf.sprintf "Usage: ./%s 0 [PORT] (server) OR ./%s 1 HOST [PORT] (client)\n" program_name program_name

let get_time = 
  let time = Int64.to_string (Mtime.to_uint64_ns (Mtime_clock.now ())) in
  let padding = String.make (max 0 (20 - String.length time)) '0' in
  (padding ^ time)

let formatted_msg t msg =
  get_time ^ t ^ msg

let converse(socket) =  (*think about flushing outputs*)
  let input = Lwt_io.of_fd ~mode:Lwt_io.input socket in 
  let output = Lwt_io.of_fd ~mode:Lwt_io.output socket in

  let rec send ()= 
    let* msg = Lwt_io.read_line Lwt_io.stdin in (*switch to buffer instead*)
    if msg = "EXIT" then exit 0
    else
    let* () = Lwt_io.write_line output (formatted_msg "m" msg) in
    let* read_receipt = Lwt_io.read_line_opt input in 
    match read_receipt with 
    | None ->        
      let* () = Lwt_io.printl("Message not received.") in 
      Lwt.return_unit
    | Some receipt when String.length receipt > 20 && receipt.[20] = '_' -> 
      let received_time = int_of_string (get_time) in 
      let sent_time = int_of_string (String.sub msg 0 20) in
      let receipt = (String.sub receipt 20 (String.length receipt - 1)) in 
      let* () = Lwt_io.printf "%s - sent: %d, received: %d\n"
      receipt sent_time received_time in
      send ()
    | Some unformatted -> 
      Printf.printf "Unformatted receipt received: '%s'" unformatted;
      send ()
    in  

  let rec poll () = 
    let* line = Lwt_io.read_line_opt input in 
    let received_time = get_time in 
    match line with 
    | None ->  
      let* () = Lwt_io.printl("Server disconnected.") in
      exit 1          
    | Some msg when String.length msg > 20 && msg.[20] = 'm' -> 
      let msg = String.sub msg 21 ((String.length msg - 21)) in 
      let* () = Lwt_io.printf "Server: %s\n" msg in
      let* () = Lwt_io.write_line output (received_time ^ "✅") in  (* design better helper*)
      poll ()
    | Some unformatted -> 
      Printf.printf "Unformatted message received: '%s'" unformatted; 
      poll ()
 in
 Lwt.join [poll(); send()]

let validated_address (address) =
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
    Lwt.fail exc) (* possibly switch to >>= instead of let* *)

let rec listen socket =
  let handle_client (client_socket, _) =
    let* () = Lwt_io.printl "Client joined the server!" in
    let output = Lwt_io.of_fd ~mode:Lwt_io.Output client_socket in
    let* () = Lwt_io.write_line output (formatted_msg "m" "Welcome to the server!") in
    Lwt.async (fun () -> converse client_socket); (* could switch to Lwt.pick if only allowing one client*)
    listen socket in 
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

let run mode host port =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.setsockopt socket Unix.TCP_NODELAY true; 
  let address = if mode <> 0 then Unix.inet_addr_any else (validated_address host) in
  let sockaddr = Unix.ADDR_INET (address, port) in
  match mode with
  | 0 -> (* server *)
    let* () = Lwt_unix.bind socket sockaddr in 
    Lwt_unix.listen socket 10;
    let* () = Lwt_io.printlf "Server running! (Port: %d)" port in 
    listen socket
  | 1 -> (* client *)
    let* () = connect socket sockaddr in
    converse socket
  | _ ->  
    Printf.printf "%s" usage_msg;
    exit 1

let start =
  let argc = Array.length Sys.argv in
  if argc < 2 then
    Printf.printf "%s" usage_msg
  else
    let mode = Sys.argv.(1) in
    match (int_of_string_opt mode) with
    | Some mode when mode = 0 || mode = 1 ->
        let default_port = "8080" in          
        let default_host = "127.0.0.1" in
        let (host, port) = 
          match mode, argc with
          | _,2 -> (default_host, default_port)  (* server with default port OR client with default host and default port*)
          | 0,3 -> (default_host, Sys.argv.(2))  (* server with specific port *)
          | 1,3 -> (Sys.argv.(2), default_port)  (* client with specific host, default port *)
          | 1,4 -> (Sys.argv.(2), Sys.argv.(3))  (* client with specific host and port *)
          | _,_ -> 
            Printf.printf "%s" usage_msg;
            exit 1
        in
        Lwt_main.run (run mode host (int_of_string port))
    | _ ->
        Printf.printf "%s" usage_msg

let () = start
