(* I put those functions here and not in common.ml to try to avoid
 * as much as possible dependencies in common.ml so can more easily
 * make ocaml script that just do a load common.ml without the need
 * to load many other files (like dumper.ml, or ANSITerminal.ml and
 * other recursive dependencies).
 * 
 * Note that can still use the function from a open Common. Don't
 * need to do a 'open Common_extra'. Loading the commons.cma is
 * enough to make the connexions.
 *)



let execute_and_show_progress len f = 
  let _count = ref 0 in
  (* kind of continuation passed to f *)
  let continue_pourcentage () = 
    incr _count;
    ANSITerminal.set_cursor 1 (-1);
    ANSITerminal.printf [] "%d / %d" !_count len; flush stdout;
  in
  ANSITerminal.printf [] "0 / %d" len; flush stdout;
  f continue_pourcentage;
  Common.pr2 ""


let _init_execute = 
  begin
    Common._execute_and_show_progress_func := execute_and_show_progress;
  end

