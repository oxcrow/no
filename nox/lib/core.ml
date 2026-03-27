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

(* Is value a Some or None? *)
let isSome value = match value with Some _ -> true | None -> false
