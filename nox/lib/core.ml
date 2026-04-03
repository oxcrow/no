(* Current position of our code *)
external uPOS : string * int * int * int = "%loc_POS"

(* Error report data *)
(* Store arbitrary data in the error report using Any (an existential wrapper) *)
type 'a uErrorReport = { message : string; source : uLocation; error : any option }
and uLocation = { file : string; line : int; start : int; end' : int }
and any = Any : 'a -> any

(* Error report exception *)
exception Report of any uErrorReport

(* Create location record *)
let xSOURCE (position : string * int * int * int) : uLocation =
  let file, line, start, end' = position in
  { file; line; start; end' }
;;

(* Extract filename from location *)
let xFILE location = match location with l -> l.file

(* Extract line number from position *)
let xLINE location = match location with l -> l.line

(* Raise an error report stating that something is not yet implemented *)
let xTODO position message =
  raise
    (Report
       {
         message = Printf.sprintf "(todo (%s))" message;
         source = xSOURCE position;
         error = None;
       })
;;

(* Raise an error report stating that unreachable condition is executed *)
let xNEVER position message =
  raise
    (Report
       {
         message = Printf.sprintf "(wtf? (%s))" message;
         source = xSOURCE position;
         error = None;
       })
;;

(* Extract data from Option if it exists; or raise error *)
(* Use as: data |> xSOME uPos *)
let xSOME position value =
  match value with
  | Some x -> x
  | None ->
      raise
        (Report
           {
             message = "Unable to unwrap optional value. (Value is None)";
             source = xSOURCE position;
             error = None;
           })
;;

let assure position condition reporter = if condition then () else reporter ()

(* Is value a Some or None? *)
let isSome value = match value with Some _ -> true | None -> false

(* Last element of a list *)
let last list = List.nth list (List.length list - 1)

(* First element of a list *)
let first list = List.hd list

(* Run executable command *)
let runShellCmd (command : string list) (input : string option) =
  (* Extract the executable and arguments *)
  let exe = List.hd command in
  let arg = command |> Array.of_list in
  (* Create pipes to read and write data to the process *)
  let readPipe, writePipe = Unix.pipe () in
  (* Execute the command with our custom pipe as its stdin *)
  let pid = Unix.create_process exe arg readPipe Unix.stdout Unix.stderr in
  (* Close the read pipe, write to the write pipe, and then close the write pipe *)
  (* NOTE: Close the write pipe so the process sees EOF *)
  Unix.close readPipe;
  (match input with
  | Some input ->
      let _ = Unix.write_substring writePipe input 0 (String.length input) in
      Unix.close writePipe
  | None -> ());
  (* Wait for the command to finish, so we can get the exit status *)
  (* BUG: The code hangs when we try to wait. WHY? Let's use WNOHANG for now! *)
  let _, exitStatus = Unix.waitpid [ WNOHANG ] pid in
  match exitStatus with
  | WEXITED 0 -> ()
  | WEXITED n -> failwith "Shell command crashed!"
  | _ -> failwith "Shell command crashed badly!"
;;
