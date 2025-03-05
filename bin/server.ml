open Lwt
open Lwt_io
open Lwt_unix

let ( let* ) = Lwt.bind
let port = 8080

let converse (input, output) = 
  let rec poll_messages () = 
    Lwt.catch (fun () ->
      let* line = Lwt_io.read_line_opt input in 
      match line with 
      | None ->  
        Lwt_io.printl("Client disconnected.")
      | Some msg -> 
        match msg with 
        | "SHUTDOWN" -> Lwt_io.printl("EXITING...")
        | _ -> 
          let* () = Lwt_io.printf "Client: %s\n" msg in
          let* message =  Lwt_io.read_line Lwt_io.stdin in 
          let* () = Lwt_io.write_line output message in
          let* () = Lwt_io.flush Lwt_io.stdout in
          poll_messages ())  
      (fun exc ->
        Lwt_io.printlf "Client error: %s" (Printexc.to_string exc))
  in poll_messages ()

let rec listen socket = 
  let* result = Lwt.catch
    (fun () -> 
      let* client = Lwt_unix.accept socket in 
      Lwt.return_some client)
    (fun exc -> 
      Lwt_io.printlf "Error accepting connection: %s" (Printexc.to_string exc) 
      >>= fun () -> Lwt.return_none) 
  in match result with 
  | None -> listen socket
  | Some (client_socket, _) -> 
    let input = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in 
    let output = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in 
    Lwt.async (fun () -> converse (input, output));
    listen socket

let run port = 
  let socket_address = Unix.(ADDR_INET (inet_addr_any, port)) in 
  let server_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in (** maybe switch to ipv6 *)
  Lwt_unix.setsockopt server_socket Unix.SO_REUSEADDR true;
  Lwt_unix.setsockopt server_socket Unix.TCP_NODELAY true;

  let* () = Lwt_unix.bind server_socket socket_address in 
  Lwt_unix.listen server_socket 10;
  let* () = Lwt_io.printlf "Server running! (Port: %d)" port in 
  listen server_socket

let () = 
  Lwt_main.run (run port)

  

