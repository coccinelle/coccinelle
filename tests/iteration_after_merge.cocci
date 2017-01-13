virtual after_start

@initialize:ocaml@
@@

let iteration_done = ref false
let tbl = Hashtbl.create 101

let inc f =
  match
    try Some (Hashtbl.find tbl f)
    with Not_found -> None
  with
    None -> Hashtbl.add tbl f 1
  | Some count -> Hashtbl.replace tbl f (succ count)

let merge_hashtbls op target source =
  let add_item key value =
    match
      try Some (Hashtbl.find target key)
      with Not_found -> None
    with
      None -> Hashtbl.add target key value
    | Some value' -> Hashtbl.replace target key (op value value') in
  Hashtbl.iter add_item source

@s@
identifier f;
position p;
@@
f@p(...);

@script:ocaml@
f << s.f;
p << s.p;
@@

inc (f, (List.hd p).file)

@e depends on after_start exists@
identifier virtual.most_used_function;
expression x;
position p;
@@

x@p = most_used_function(...)

@script:python@
p << e.p;
@@

print("{}: {}".format(p[0].file, p[0].line))

@finalize:ocaml@
tbls << merge.tbl;
@@
if not !iteration_done then
  begin
    iteration_done := true;
    let tbl =
      match tbls with
        [] -> failwith "Unexpected empty table list"
      | tbl :: others ->
          List.iter (merge_hashtbls ( + ) tbl) others;
          tbl in
    let (most_useds, _) =
      Hashtbl.fold (fun k v accu ->
        let (l, max) = accu in
        if v > max then ([k], v)
        else if v = max then (k :: l, v)
        else accu) tbl ([], 0) in
    List.iter (fun (fn, file) ->
      let it = new iteration () in
      it#set_files [file];
      it#add_virtual_rule After_start;
      it#add_virtual_identifier Most_used_function fn;
      it#register ()) most_useds
  end
