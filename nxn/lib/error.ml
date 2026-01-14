open Utils.String

type error_report = { message : string }

exception Report of error_report

let write = print_endline
let fail message = raise (Report { message })
let lok loc = hot (" Raised? [" ^ loc ^ "]")

module Failure = struct
  let error loc x = fail @@ " " ^ red (cross sp) ^ x ^ sp ^ lok loc

  let never loc x =
    error loc
      ("Unreachable code executed!" ^ nl ^ sp ^ dot (sp ^ String.trim x) ^ nl)
  ;;

  let todo loc x =
    error loc ("Not yet implemented!" ^ nl ^ sp ^ dot (sp ^ String.trim x) ^ nl)
  ;;

  let warn loc x = write @@ "Warning: " ^ x ^ sp ^ paren loc

  (* Verification methods of different types *)
  let ensure loc c = if c = true then () else error loc "Assert failed."
  let verify loc c x = if c then () else error loc ("Assert failed: " ^ x)
  let assure loc c x = if c then () else error loc (x ())
end

module Unwrap = struct
  let some loc x =
    match x with Some y -> y | None -> fail @@ "None." ^ sp ^ paren loc
  ;;

  let ok loc x =
    match x with Ok y -> y | Error _ -> fail @@ "Error." ^ sp ^ paren loc
  ;;
end
