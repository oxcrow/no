(* Current position of our code *)
external uPOS : string * int * int * int = "%loc_POS"

(* Error report data *)
type uReport = { message : string; position : uLocation }
and uLocation = { file : string; line : int; start : int; end' : int }

(* Error report exception *)
exception Report of uReport

(* Create location record *)
let xLOC (pos : string * int * int * int) : uLocation =
  let file, line, start, end' = pos in
  { file; line; start; end' }
;;

(* Extract filename from location *)
let xFILE loc = match loc with l -> l.file

(* Extract line number from position *)
let xLINE loc = match loc with l -> l.line
