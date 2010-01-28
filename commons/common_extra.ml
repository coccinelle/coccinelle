(* I put those functions here and not in common.ml to try to avoid
 * as much as possible dependencies in common.ml so I can more easily
 * make ocaml script that just do a load common.ml without the need
 * to load many other files (like dumper.ml, or ANSITerminal.ml and
 * other recursive dependencies).
 *
 * Note that you can still use the functions below from an open Common.
 * You don't need to do a 'open Common_extra'; loading the commons.cma is
 * enough to make the connexions.
 *)


(* how to use it ? ex in LFS:
 *  Common.execute_and_show_progress (w.prop_iprop#length) (fun k ->
 *  w.prop_iprop#iter (fun (p, ip) ->
 *     k ();
 *     ...
 *  ));
 *
 *)

let execute_and_show_progress len f =
  let _count = ref 0 in
  (* kind of continuation passed to f *)
  let continue_pourcentage () =
    incr _count;
    ANSITerminal.set_cursor 1 (-1);
    ANSITerminal.printf [] "%d / %d" !_count len; flush stdout;
  in
  let nothing () = () in

  ANSITerminal.printf [] "0 / %d" len; flush stdout;
  if !Common._batch_mode
  then f nothing
  else f continue_pourcentage
  ;
  Common.pr2 ""


let set_link () =
  Common._execute_and_show_progress_func := execute_and_show_progress


let _init_execute =
  set_link ()
