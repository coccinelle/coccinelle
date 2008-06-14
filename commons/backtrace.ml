open Common


(* src: jane street core lib *)
external print : unit -> unit = "print_exception_backtrace_stub" "noalloc"


exception MyNot_Found

let foo1 () =
  if 1=1 
  then raise MyNot_Found
  else 2

let foo2 () =
  foo1 () + 2

let test_backtrace () =
  (try ignore(foo2 ())
  with exn -> 
    pr2 (Common.exn_to_s exn);
    print();
    failwith "other exn"
  );
  print_string "ok cool\n";
  ()


(*
  (let s = "-test_backtrace" in s, Arg.Unit (fun () -> action := s),
  "   <file>");

  | [] when !action = "-test_backtrace" -> 
  Test.test_backtrace ()

*)


