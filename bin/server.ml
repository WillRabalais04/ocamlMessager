open Lwt
open Lwt_io
open Lwt_unix

let ( let* ) = Lwt.bind
let port = 8080

let converse (input, output) = 
  (let rec send () = 
    (let* line = Lwt_io.read_line Lwt_io.stdin in 
    let kick = 
      try Some (Scanf.sscanf line "KICK %s" (fun client -> client))
      with _ -> None
    in 
    match line, kick with 
    | "EXIT", _ ->  
      let* () = Lwt_io.close output in
      let* () = Lwt_io.close input in
      let* () = Lwt_io.printl("Shutting down server...") in 
      Lwt.return_unit
    | _,Some client -> 
      let* () = Lwt_io.printlf "Kicking client %s." client in
      let* () = Lwt_io.close output in 
      Lwt.return_unit
    | msg,_ ->     
      let* () = Lwt_io.write_line output msg in 
      (* let* () = Lwt_io.flush_all () in
      let* read_receipt = Lwt_io.read_line input in 
      let* () = Lwt_io.printlf " %s - %d" read_receipt 0 in  *)
      send()) in

  let rec poll () = 
    (Lwt.catch (fun () ->
      let* line = Lwt_io.read_line_opt input in 
      match line with 
      | None ->  
        let* () = Lwt_io.write_line output "❌" in
        let* () = Lwt_io.printl("Client disconnected.") in
        Lwt.return_unit
      | Some msg -> 
        let* () = Lwt_io.printf "Client: %s\n" msg in
        (* let* () = Lwt_io.write_line output "Received✅" in
        let* () = Lwt_io.flush_all () in  *)
        poll())  
      (fun exc ->
        let* () = Lwt_io.printlf "Client error: %s" (Printexc.to_string exc) in 
        Lwt.return_unit)) in 
  Lwt.join [poll(); send()]) 

let rec listen socket = 
  let* result = Lwt.catch
    (fun () -> 
      let* client = Lwt_unix.accept socket in 
      Lwt.return_some client) 
    (fun exc -> 
      let* ()  = Lwt_io.printlf "Error accepting connection: %s" (Printexc.to_string exc) in 
      Lwt.return_none) 
  in match result with 
  | None -> Lwt.return_unit
  | Some (client_socket, _) -> 
    let input = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in 
    let output = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in 
    let*() = printl "Client joined the server!" in 
    let* () = Lwt_io.write_line output "Welcome to the server!" in 
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

  

