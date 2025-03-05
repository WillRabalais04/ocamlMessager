open Lwt
open Lwt_io
open Lwt_unix
open Stdlib

let (let*) = Lwt.bind
let port = "8080"

let converse host port = 
  let* address_info = 
    Lwt_unix.getaddrinfo host port [Unix.(AI_FAMILY PF_INET)] in 
  let* address = match address_info with 
  | [] -> Lwt.fail_with "Could not resolve host!"
  | address :: _ -> Lwt.return address.Unix.ai_addr 
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in 
  let* () = Lwt_unix.connect socket address in 
  let input = Lwt_io.of_fd ~mode:Lwt_io.input socket in 
  let output = Lwt_io.of_fd ~mode:Lwt_io.output socket in

  let rec send () = 
    let* message = Lwt_io.read_line Lwt_io.stdin in 
    let* () =  Lwt_io.write_line output message in 
    let* response = Lwt_io.read_line input in 
    let* () = Lwt_io.printlf "Server: %s" response in
    send ()
  in send ()

let () =
  print_string("Connected to server!");  
  let argc = Array.length Sys.argv in
  match argc with 
  | 2 -> 
    let host = (Sys.argv.(1)) in 
    Lwt_main.run (converse host port)
  | _ -> 
    print_string("Expected usage: ./client [HOST]")
  



