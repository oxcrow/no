open Core

let posOfError (pos : Lexing.position) =
  let lineIndex = pos.pos_lnum in
  let colIndex = pos.pos_cnum - pos.pos_bol + 1 in
  (lineIndex, colIndex)
;;

let scream file lineIndex colIndex =
  let lines = String.split_on_char '\n' (readFileContent file) in
  let message lineIndex =
    match lineIndex >= 1 && lineIndex < List.length lines with
    | true -> (fmt "%4d │ " lineIndex, List.nth lines (lineIndex - 1))
    | false -> (fmt "   ~ │", "")
  in
  let fmtA, lineA = message (lineIndex - 2) in
  let fmtB, lineB = message (lineIndex - 1) in
  let fmtC, lineC = message lineIndex in
  let lineX =
    let splitAt string i =
      let n = String.length string in
      match i < 0 || i > n with
      | true -> ("", string)
      | false ->
          let left = String.sub string 0 i in
          let right = String.sub string i (n - i) in
          (left, right)
    in
    let left, right = splitAt lineC colIndex in
    left ^ ansiRed ^ right ^ ansiReset
  in
  let fmtD, lineD = message (lineIndex + 1) in
  let fmtE, lineE = message (lineIndex + 2) in
  String.concat "\n" [ fmtA ^ lineA; fmtB ^ lineB; fmtC ^ lineX; fmtD ^ lineD; fmtE ^ lineE ]
;;

let quack file loc = ()
