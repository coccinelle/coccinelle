let option_bind option f =
  match option with
    None -> None
  | Some x -> f x

let option_find f x =
  try Some (f x)
  with Not_found -> None

let option_or option f =
  match option with
    Some _ -> option
  | None -> f ()

let option_unwrap option =
  match option with
    None -> raise Not_found
  | Some result -> result

let option_unwrap_default default option =
  match option with
    None -> default
  | Some x -> x

let substring_between string before after =
  String.sub string before (after - before)

let int_of_octal octal =
  int_of_string ("0o" ^ octal)

let int_of_hex hex =
  int_of_string ("0x" ^ hex)

let split_left_on_char ?(from=0) char s =
  try substring_between s from (String.index_from s from char)
  with Not_found ->
    if from = 0 then s
    else substring_between s from (String.length s)

let split_right_on_char ?(from=0) char s =
  try
    substring_between s (String.index_from s from char + 1)
      (String.length s)
  with Not_found ->
    if from = 0 then s
    else substring_between s from (String.length s)

let trim_carriage_return line =
  let length = String.length line in
  if String.sub line (length - 1) 1 = "\r" then
    String.sub line 0 (length - 1)
  else
    line

let has_prefix prefix s =
  let prefix_length = String.length prefix in
  String.length s >= prefix_length && String.sub s 0 prefix_length = prefix

let input_lines channel =
  let accu = ref [] in
  try
    while true do
      accu := trim_carriage_return (input_line channel) :: !accu;
    done;
    assert false
  with End_of_file ->
    List.rev !accu

let try_finally f arg finally finally_arg =
  try
    let result = f arg in
    finally finally_arg;
    result
  with e ->
    finally finally_arg;
    raise e

let read_and_close channel f arg =
  try
    let result = f arg in
    close_in channel;
    result
  with e ->
    close_in_noerr channel;
    raise e

let write_and_close channel f arg =
  try
    let result = f arg in
    close_out channel;
    result
  with e ->
    close_out_noerr channel;
    raise e

let with_temp_file contents f =
  let (file, channel) = Filename.open_temp_file "pyml_tests" ".py" in
  try_finally begin fun () ->
    write_and_close channel (output_string channel) contents;
    let channel = open_in file in
    read_and_close channel (f file) channel
  end ()
    Sys.remove file

let with_pipe f =
  let (read, write) = Unix.pipe () in
  let in_channel = Unix.in_channel_of_descr read
  and out_channel = Unix.out_channel_of_descr write in
  try_finally (f in_channel) out_channel
    (fun () ->
      close_in_noerr in_channel;
      close_out_noerr out_channel) ()

let with_stdin_from channel f arg =
  let stdin_backup = Unix.dup Unix.stdin in
  Unix.dup2 (Unix.descr_of_in_channel channel) Unix.stdin;
  try_finally
    f arg
    (Unix.dup2 stdin_backup) Unix.stdin

let with_channel_from_string s f =
  with_pipe begin fun in_channel out_channel ->
    output_string out_channel s;
    close_out out_channel;
    f in_channel
  end

let with_stdin_from_string s f arg =
  with_channel_from_string s (fun channel -> with_stdin_from channel f arg)
