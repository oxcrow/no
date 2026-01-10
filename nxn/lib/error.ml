open Utils.String

let write = print_endline
let fail = failwith

module Failure = struct
  let todo loc x = fail @@ "Not yet implemented: " ^ x ^ sp ^ paren loc
  let never loc x = fail @@ "Unreachable code executed: " ^ x ^ sp ^ paren loc
  let error loc x = fail @@ "Error: " ^ x ^ sp ^ paren loc
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
