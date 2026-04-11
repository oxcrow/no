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
  let command = String.concat " " command in
  let input = match input with Some text -> text | _ -> "" in
  (* Read data from a channel, line by line *)
  let readChannel channel : string =
    let rec readLine channel =
      try
        let line = Stdlib.input_line channel in
        line :: readLine channel
      with End_of_file -> []
    in
    String.concat "\n" (readLine channel)
  in
  let stdoutChan, stdinChan, stderrChan =
    Unix.open_process_full command Unix.(environment ())
  in
  (* Send input to stdin *)
  (* NOTE: IMMEDIATELY flush and close the channel, so EOF is sent. *)
  Stdlib.output_string stdinChan input;
  flush stdinChan;
  close_out stdinChan;
  (* Read data from the output and error channels *)
  let stdout = readChannel stdoutChan in
  let stderr = readChannel stderrChan in
  (* Clean up resources and return result *)
  let status = Unix.close_process_full (stdoutChan, stdinChan, stderrChan) in
  (match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> failwith ("Command terminated with error code: " ^ string_of_int n)
  | _ -> xNEVER uPOS "Command terminated with error.");
  (stdout, stderr)
;;
