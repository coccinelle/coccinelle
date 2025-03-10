@initialize:ocaml@
@@

let n = ref 0

let inc n = n:= !n+1; !n

@script:ocaml@
@@

let fl = List.hd (Coccilib.files()) in
let pieces = String.split_on_char '/' fl in
if List.mem "tools" pieces || List.mem "scripts" pieces
then Coccilib.exit()

@rr@
iterator i = {list_for_each,list_for_each_rcu,list_for_each_continue,list_for_each_prev,list_for_each_safe,list_for_each_prev_safe,list_for_each_entry,list_for_each_entry_reverse,list_for_each_entry_continue,list_for_each_entry_continue_reverse,list_for_each_entry_from,list_for_each_entry_from_reverse,list_for_each_entry_safe,list_for_each_entry_safe_continue,list_for_each_entry_safe_from,list_for_each_entry_safe_reverse,hlist_for_each,hlist_for_each_safe,hlist_for_each_entry,hlist_for_each_entry_continue,hlist_for_each_entry_from,hlist_for_each_entry_safe};
statement S;
position p;
@@

i@p(...) S

@script:ocaml ss@
_p << rr.p;
id;
@@

let str = Printf.sprintf "__a__%d" (inc n) in
id := make_ident str

@@
iterator i = {list_for_each,list_for_each_rcu,list_for_each_continue,list_for_each_prev,list_for_each_safe,list_for_each_prev_safe,list_for_each_entry,list_for_each_entry_reverse,list_for_each_entry_continue,list_for_each_entry_continue_reverse,list_for_each_entry_from,list_for_each_entry_from_reverse,list_for_each_entry_safe,list_for_each_entry_safe_continue,list_for_each_entry_safe_from,list_for_each_entry_safe_reverse,hlist_for_each,hlist_for_each_safe,hlist_for_each_entry,hlist_for_each_entry_continue,hlist_for_each_entry_from,hlist_for_each_entry_safe};
identifier ss.id;
statement S;
position rr.p;
@@

+{ int id = 0;
i@p(...)
+ {
+   id++;
    S
++ }
++ trace_printk("%s:%d, count: %d\n", __FILE__, __LINE__, id);
++ }
