(** Read the contents of file into a string. *)
let read_file_content filename : string =
  let file = open_in_bin filename in
  let file_length = in_channel_length file in
  let content = really_input_string file file_length in
  content
;;
