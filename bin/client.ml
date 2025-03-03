open Lwt
open Lwt_io
open Lwt_unix

let (let*) = Lwt.bind

let run host port = 
  let* address_info = 
    Lwt_unix.getaddrinfo host (string_of_int port) [Unix.(AI_FAMILY PF_INET)] in 
  let* address = match address_info with 
  | [] -> Lwt.fail_with "Could not resolve host!"
  | address :: _ -> Lwt.return address.Unix.ai_addr 
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in 
  let* () Lwt_unix.connect socket address in 
  let input = Lwt_io.of_fd ~mode:Lwt_io.input socket in 
  let output = Lwt_io.of_fd ~mode:Lwt_io.output socket in

  let rec send () = 
    let* message = Lwt_io.read_line Lwt_io.stdin in 
    let* () Lwt_io.write_line output message in 
    let* response = Lwt_io.read_line input in 
    let* () = Lwt_io.printlf "Server: %s" response in
    send ()
  in send ();;
  



