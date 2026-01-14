(** Tuple utils *)
module Tuple = struct
  let first t = match t with a, _ -> a
  let second t = match t with _, b -> b
end

(** String utils *)
module String = struct
  let sp = " "
  let nl = "\n"
  let sp2 = "  "
  let sp3 = "   "
  let quote s = "\"" ^ s ^ "\""
  let paren s = "(" ^ s ^ ")"
  let black s = "\027[0m" ^ s
  let gray s = "\027[2m" ^ s ^ black ""
  let red s = "\027[91m" ^ s ^ black ""
  let cross s = "✕" ^ s
  let dot s = "●" ^ s
  let hot s = "○" ^ s
  let cot s = "╰" ^ s
end
