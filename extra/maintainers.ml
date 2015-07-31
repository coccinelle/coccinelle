open Common


type subsystem_info = subsystem list
   and subsystem = Subsystem of (dir * maintainers) *
                                (dir * maintainers) list (* subdirs *)
      and dir = string
      and maintainers = string list


let mk_inverted_index_subsystem xs =
  let h = Hashtbl.create 101 in
  xs +> List.iter (function (Subsystem ((leader, emails), dirs)) ->
    Hashtbl.add h leader leader;
    dirs +> List.iter (fun (subdir, emails) ->
      Hashtbl.add h subdir leader
    ));
  h

let subsystem_to_assoc xs =
  xs +> List.map (function (Subsystem ((s, emails), ys)) -> s, (emails, ys))
let subsystem_to_hash xs =
  xs +> subsystem_to_assoc +> Common.hash_of_list

let all_dirs_from_subsystem_info xs =
  xs +> List.map (function (Subsystem ((s, emails), dirs)) ->
                    s::(List.map fst dirs)
  ) +> Common.union_all



let unparse_subsystem_info xs filename =
  Common.with_open_outfile filename (fun (pr_no_nl,chan) ->
    let pr s = pr_no_nl (s ^ "\n") in

    xs +> List.iter (function Subsystem ((s, emails), ys) ->
      pr (sprintf "%-40s : %s" s (Common.join " ," emails));
      ys +> List.iter (fun (s, emails) ->
        pr (sprintf "    %-40s : %s" s (Common.join " ," emails));
      );
      pr "";
    )
  )

let parse_subsystem_info filename =
  let xs = cat filename in
  let xs = xs +> List.map (Str.global_replace (Str.regexp "#.*") "" ) in
  let xs = xs +> List.filter (fun s -> not (s =~ "^[ \t]*$")) in

  (* split by header of section *)
  let xs = xs +> Common.split_list_regexp "^[^ ]" in

  xs +> List.map (fun (s, xs) ->
    assert (s =~ "^\\([^ ]+\\) *: *\\(.*\\)");
    let (dir, email) = matched2 s in
    let emails = Common.split "[ ,]+" email in
    let group = xs +> List.map (fun s ->
      assert (s =~ "^[ ]+\\([^ ]+\\) *: *\\(.*\\)");
      let (dir, email) = matched2 s in
      let emails = Common.split "[ ,]+" email in
      (dir, emails)
    ) in
    Subsystem ((dir, emails), group)
  )


let generate_naive_subsystem_info dirs =
  let dirs' = dirs +> List.map (fun s -> Common.split "/" s, s ) in

  let rec aux_dirs xs =
    match xs with
    | [] -> []
    | (dir_elems,s)::xs ->
        let cond, base =
          if List.length dir_elems >= 2 then
            let base = Common.take 2 dir_elems in
            (fun dir_elems' ->
              List.length dir_elems' >= 2 && Common.take 2 dir_elems' = base),
            base
          else
            (fun dir_elems' -> dir_elems' = dir_elems),
            dir_elems
        in

        let (yes, no) = xs +> Common.partition_either (fun (dir_elems', x) ->
          if cond dir_elems'
          then Left (x, [])
          else Right (dir_elems', x)
        ) in
        (Subsystem ((s, [""]), yes))::aux_dirs no
  in
  aux_dirs dirs'
(* old: dirs  +> List.map (fun s ->  Subsystem (s, "", [])) *)


(* a = current info file, in general manually extended; b = generated one *)
let check_up_to_date a b  =
  let set1 = all_dirs_from_subsystem_info a in
  let set2 = all_dirs_from_subsystem_info b in
  (set1 $-$ set2) +> List.iter (fun s ->
    pr2 ("old directory disappeared: " ^ s)
  );
  (set2 $-$ set1) +> List.iter (fun s ->
    pr2 ("new directory appeared: " ^ s)
  )
