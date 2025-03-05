@initialize:ocaml@
@@

let iteration_done = ref false
let v = 1

@s@
identifier f;
position p;
@@
f@p(...);

@script:ocaml@
p << s.p;
@@

if not !iteration_done then
  begin
    iteration_done := true;
    let it = new iteration () in
    it#set_files [(List.hd p).file];
    it#register ()
  end

@finalize:ocaml@
v << merge.v;
@@
Printf.printf "%d\n" (List.length v)