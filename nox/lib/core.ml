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
let xSOURCE (pos : string * int * int * int) : uLocation =
  let file, line, start, end' = pos in
  { file; line; start; end' }
;;

(* Extract filename from location *)
let xFILE loc = match loc with l -> l.file

(* Extract line number from position *)
let xLINE loc = match loc with l -> l.line

(* Extract data from Option if it exists; or raise error *)
(* Use as: data |> xSOME uPos *)
let xSOME pos value =
  match value with
  | Some x -> x
  | None ->
      raise
        (Report
           {
             message = "Unable to unwrap optional value. (Value is None)";
             source = xSOURCE pos;
             error = None;
           })
;;

(* Is value a Some or None? *)
let isSome value = match value with Some _ -> true | None -> false
