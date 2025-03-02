open Lwt
open Lwt_io
open Lwt_unix

let ( let* ) = Lwt.bind

let server_socket = Lwt_unix.inet6_addr_loopback;

let handle_client (input, output) = 
  let rec poll_messages () = 
    Lwt.catch (fun () ->
      let* line = Lwt_io.read_line_opt input in 
      match line with 
      | None ->  
        Lwt_io.printl("Client disconnected.")
      | Some msg -> 
        let* () = Lwt_io.write_line output msg in 
        poll_messages ()) 
      (fun exc ->
        Lwt_io.printl "Client error: %s" (Printexc.to_string exc))
  in poll_messages ()

let rec listen socket = 
  let* result = Lwt.catch
    (fun () -> 
      Lwt_unix.accept server_socket)
    (fun exc -> 
      Lwt_io.printlf "Error accepting connection: %s" (Printexc.to_string exc) 
        >>= fun () ->
        Lwt.return_none ())
  in match result with 
  | None -> listen socket
  | Some (client_socket, _) -> 
    let input = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in 
    let output = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in 
    handle_client (input, output) in 
    Lwt.async (fun () -> handle_client (input, output));
    listen socket

let run port = 
  let socket_address = Unix.(ADDR_INET (inet_addr_any, port)) in 
  let server_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in (** maybe switch to ipv6 *)
  Lwt_unix.setsockopt server_socket Unix.SO_REUSEADDR true;
  Lwt_unix.setsockopt server_socket Unix.TCP_NODELAY true;

  let* () = Lwt_unix.bind server_socket sockaddr in 
  Lwt_unix.listen server_socket 10;
  let* () = Lwt_io.printlf "Server running! (Port: %d)" port in 
  listen server_socket

let () = 
  Lwt_main.run (run 12345)

  

