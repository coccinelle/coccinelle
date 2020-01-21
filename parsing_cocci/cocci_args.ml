let get_cocci_files arglist =
  let cocci_files = ref [] in
  let add_if_cocci_file arg =
    let len = String.length arg in
    if len >= 6 && String.sub arg (len - 6) 6 = ".cocci"
    then cocci_files := arg :: !cocci_files in
  let rec get_cocci_files = function
    | [] -> ()
    | f::fs when Sys.is_directory f ->
        let filenames = Sys.readdir f in
        let filespaths = Array.map (Filename.concat f) filenames in
        get_cocci_files (Array.to_list filespaths @ fs)
    | filename::fs ->
        add_if_cocci_file filename;
        get_cocci_files fs in
  let rec find_cocci_files = function
    | [] -> ()
    | "--cocci-file"::filename::args
    | "--sp-file"::filename::args ->
        cocci_files := filename :: !cocci_files;
        find_cocci_files args
    | "--test"::testname::args ->
        let filename = "tests/" ^ testname ^ ".cocci" in
        cocci_files := filename :: !cocci_files;
        find_cocci_files args
    | "--testall"::args
    | "--test-spacing"::args ->
        get_cocci_files ["tests"];
        find_cocci_files args
    | arg::args
      (* this pattern supposes no one tries to do manipulations like
       * '--some_arg --sp-file --some_other_arg' with --sp-file being
       * --some_arg's argument on the command line
       *
       * Note: I (Coco) only allowed my code to do that because it is
       * called by code that already relies on this (wrong) principle
       *) ->
        add_if_cocci_file arg;
        find_cocci_files args in
  find_cocci_files arglist;
  List.rev !cocci_files

let read_arg_line line =
  let in_single_quote = ref false in
  let in_double_quote = ref false in
  let arg_start = ref 0 in
  let res = ref [] in
  let current_arg = ref "" in
  let process_quotes index in_current_quote in_other_quote =
    if !in_current_quote
    then
      begin
        if index > !arg_start
        then
          current_arg :=
            !current_arg ^ String.sub line !arg_start (index - !arg_start);
        arg_start := index + 1;
        in_current_quote := false
      end
    else if not !in_other_quote
    then
      begin
        arg_start := index + 1;
        in_current_quote := true
      end in
  let line_len = String.length line in
  for i = 0 to line_len - 1 do
    (* delimiters are :
      * a blank character (space, tab, newline) (not in a string)
      * a single quote (outside of a double quoted string and not escaped)
      * a double quote (outside of a single quoted string and not escaped)
    *
    * Anything else is part of an argument
    *)
    if (line.[i] = ' ' || line.[i] = '\t' || line.[i] = '\n')
       && not !in_single_quote && not !in_double_quote
    then
      begin
        if i <> !arg_start || !current_arg <> ""
        then
          begin
            let arg =
              !current_arg ^ String.sub line !arg_start (i - !arg_start) in
            current_arg := "";
            res := arg::!res
          end;
        arg_start := i + 1
      end
    else if line.[i] = '\'' && (i = 0 || line.[i - 1] <> '\\')
    then
      process_quotes i in_single_quote in_double_quote
    else if line.[i] = '"' && (i = 0 || line.[i - 1] <> '\\')
    then
      process_quotes i in_double_quote in_single_quote
  done;
  if !in_single_quote || !in_double_quote
  then failwith "Unmatched quote";
  if !arg_start < line_len
  then
    current_arg :=
      !current_arg ^ String.sub line !arg_start (line_len - !arg_start);
  if !current_arg <> "" || line.[line_len - 1] = '\''
     || line.[line_len - 1] = '"'
  then res := !current_arg :: !res;
  List.rev !res

let read_cocci_args filename =
  let file = open_in filename in
  let rec loop acc =
    try
      let line = input_line file in
      let line = String.trim line in
      let len = String.length line in
      if len > 7 (* more than '#spatch' *) && line.[0] = '#'
      then
        let sub_line = String.sub line 1 (len - 1) in
        let sub_line = String.trim sub_line in
        let sub_len = String.length sub_line in
        if sub_len > 6 (* more than 'spatch' *)
           && String.sub sub_line 0 6 = "spatch"
        then
          let args = read_arg_line (String.sub sub_line 6 (sub_len - 6)) in
          loop (args::acc)
        else
          loop acc
      else
        loop acc
    with End_of_file -> List.rev acc in
  let args = loop [] in
  List.flatten args

let prepend arglist =
  let cocci_files = get_cocci_files arglist in
  let args = List.flatten (List.map read_cocci_args cocci_files) in
  (* arglist's first element is the binary name *)
  List.hd arglist :: args @ List.tl arglist
