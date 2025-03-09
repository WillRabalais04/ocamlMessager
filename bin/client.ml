open Lwt
open Lwt_io
open Lwt_unix
open Stdlib

let (let*) = Lwt.bind
let port = "8080"

let converse(input, output) = 
  let rec send () = 
    let* line = Lwt_io.read_line Lwt_io.stdin in 
    match line with 
    | "EXIT" ->  
      let* () = Lwt_io.close output in
      let* () = Lwt_io.close input in
      let* () = Lwt_io.printl("Exiting...") in
      Lwt.return_unit 
    | msg ->     
      (let* () = Lwt_io.write_line output msg in 
      let* () = Lwt_io.flush output in
      (* (let* () = Lwt_io.write_line output msg in 
      let* read_receipt = Lwt_io.read_line input in 
      let* () = Lwt_io.printlf "%s - %d" read_receipt 0 in *)
      send()) in  

  let rec poll () = 
    (let* line = Lwt_io.read_line_opt input in 
    match line with 
    | None ->  
      (* let* () = Lwt_io.write_line output "❌" in *)
      let* () = Lwt_io.printl("Server disconnected.") in 
      Lwt.return_unit
    | Some msg -> 
      let* () = Lwt_io.printf "Server: %s\n" msg in
      (* let* () = Lwt_io.write_line output "✅" in
      Lwt_io.flush_all () in  *)
      poll())
 in
 Lwt.join [poll(); send()]

let run host port = 
  let* address_info = 
    Lwt_unix.getaddrinfo host port [Unix.(AI_FAMILY PF_INET)] in 
  let* address = (match address_info with 
  | [] -> 
    Lwt.fail_with "Could not resolve host!"
  | address :: _ -> Lwt.return address.Unix.ai_addr)
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in 
  let* () = Lwt_unix.connect socket address in 
  let input = Lwt_io.of_fd ~mode:Lwt_io.input socket in 
  let output = Lwt_io.of_fd ~mode:Lwt_io.output socket in
  converse (input, output)

let () =
  let argc = Array.length Sys.argv in
  match argc with 
  | 2 -> 
    let host = (Sys.argv.(1)) in 
    Lwt_main.run (run host port)
  | _ -> 
    print_string("Expected usage: ./client [HOST]")
  