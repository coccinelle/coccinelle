open Common



type kbuild_info = directory list
   and directory = Directory of string (*dirname*) * group list
   and group = Group of filename list

let directories_to_assoc xs =
  xs +> List.map (function (Directory (s, ys)) -> s, ys)
let directories_to_hash xs =
  xs +> directories_to_assoc +> Common.hash_of_list
let files_of_groups xs =
  xs +> List.map (function Group ys -> ys) +> Common.union_all



let adjust_dirs dirs =
  dirs +> Common.map_filter (fun s ->
    match s with
    | s when s =~ "^\\.$" -> None
    | s when s =~ "^\\./\\.git" -> None
    | s when s =~ "^\\./\\.tmp_versions" -> None
    | s when s =~ "^\\./include/config/" -> None
    | s when s =~ "^\\./usr/include" -> None
    | s when s =~ "^\\./\\(.*\\)" -> Some (matched1 s)
    | s -> Some s
  )



let unparse_kbuild_info xs filename =
  Common.with_open_outfile filename (fun (pr_no_nl,chan) ->
    let pr s = pr_no_nl (s ^ "\n") in

    xs +> List.iter (function Directory (s, ys) ->
      pr s;
      ys +> List.iter (function Group zs ->
        pr ("  " ^ (join " " zs));
      );
      pr "";
    )
  )

let parse_kbuild_info filename =
  let xs = cat filename in
  let xs = xs +> List.map (Str.global_replace (Str.regexp "#.*") "" ) in
  let xs = xs +> List.filter (fun s -> not (s =~ "^[ \t]*$")) in

  (* split by header of section *)
  let xs = xs +> Common.split_list_regexp "^[^ ]" in

  xs +> List.map (fun (s, xs) ->
    let groups = xs +> List.map (fun s ->
      assert (s =~ "^[ ]+\\(.*\\)");
      let files = matched1 s in
      let cfiles = Common.split " +" files in
      Group cfiles
    ) in

    Directory (s, groups)
  )

let generate_naive_kbuild_info dirs =
    dirs  +> List.map (fun s ->
      let files = Common.readdir_to_file_list s in
      let files_ext = files +> List.map Common.dbe_of_filename_safe in
      let cfiles = files_ext +> Common.map_filter
        (function
        | Left (d,base, "c") ->
            if base =~ ".*\\.mod$" then None
            else Some base
        | _ -> None
        ) in
      let ys = cfiles +> List.map (fun c -> Group [c ^ ".c"]) in
      Directory (s, ys)
    )




let generate_kbuild_info_from_depcocci dirs outfile =
  Common.with_open_outfile outfile (fun (pr_no_nl, chan) ->
    dirs  +> List.iter (fun s ->
      pr_no_nl (s ^ "\n");
      let depcocci = Common.cat (Filename.concat s "depcocci.dep") in
      depcocci +> List.iter (fun s -> pr_no_nl (s ^ "\n"));
      pr_no_nl "\n";
    )
  )
(*
    dirs  +> List.map (fun s ->
      let groups = depcocci +> List.map (fun s -> Group (Common.split " +" s))
      in
      Directory (s, groups)
    )
*)


type makefile =
  {
    obj_dirs : string stack ref;
    obj_config: (string list) stack ref;
    obj_objs: (string * (string list)) stack ref;
  }
let empty_makefile () =
 failwith "empty_makefile"

let parse_makefile file =
  let xs = Common.cat file in
  let s = Common.unlines xs in
  let s = Str.global_replace (Str.regexp "\\\\\n") "" s in
  let xs = Common.lines_with_nl s in
  let xs = xs +> List.map (Str.global_replace (Str.regexp "#.*") "" ) in
  let xs = xs +> List.filter (fun s -> not (s =~ "^[ \t]*$")) in
  let _m = empty_makefile () in

  xs +> List.iter (fun s ->
    match s with
    | s when s =~ "obj-\\$(CONFIG_.*)[ \t]*[\\+:]=\\(.*/\\)" ->
        pr2_no_nl ("DIR: " ^ s)
    | s when s =~ "obj-y[ \t]*\\+=\\(.*/\\)" ->
        pr2_no_nl ("DIR: " ^ s)
    | s when s =~ "obj-\\$(CONFIG_.*)[ \t]*[\\+:]=\\(.*\\)" ->
        let s = matched1 s in
        let objs = Common.split "[ \t]+" s in
        assert(List.for_all (fun s -> thd3 (Common.dbe_of_filename s) =$= "o")
                  objs);

        pr2 ("OBJS: " ^ (join "|" objs))

    | s when s =~ "[a-zA-Z0-9_]+-objs[ \t]*[\\+:]=\\(.*\\)" ->
        let s = matched1 s in
        let objs = Common.split "[ \t]+" s in

        pr2 ("OBJSMODULE: " ^ (join "|" objs))

    | s  ->
        pr2_no_nl ("OTHER: " ^ s)

  )


let generate_less_naive_kbuild_info dirs =
    dirs  +> List.map (fun s ->
      let files = Common.readdir_to_file_list s in
      let files_ext = files +> List.map Common.dbe_of_filename_safe in
      let cfiles = files_ext +> Common.map_filter
        (function
        | Left (d,base, "c") ->
            if base =~ ".*\\.mod$" then None
            else Some base
        | _ -> None
        ) in
      match cfiles with
      | [] -> Directory (s, [])
      | _::_ ->
          if Common.lfile_exists (Filename.concat s "Makefile")
          then
            let _res = parse_makefile (Filename.concat s "Makefile") in
            let ys = cfiles +> List.map (fun c -> Group [c ^ ".c"]) in
            Directory (s, ys)
          else
            failwith ("no Makefile found in: " ^ s)

    )



(* a = current info file, in general manually extended; b = generated one *)
let check_up_to_date a b  =
  let das = directories_to_assoc a in
  let dbs = directories_to_assoc b in
  let all_dirs = (das +> List.map fst) $+$ (dbs +> List.map fst) in
  all_dirs +> List.iter (fun dir ->
    match
      optionise (fun () -> List.assoc dir das),
      optionise (fun () -> List.assoc dir dbs)
    with
    | None, None -> raise (Impossible 57)
    | None, Some gbs -> pr2 ("new directory appeared:" ^ dir)
    | Some gas, None -> pr2 ("old directory disappeared:" ^ dir)
    | Some gas, Some gbs ->
        let afiles = files_of_groups gas in
        let bfiles = files_of_groups gbs in
        let all_files = afiles $+$ bfiles in
        all_files +> List.iter (fun file ->
          match List.mem file afiles, List.mem file bfiles with
          | false, false -> raise (Impossible 58)
          | false, true -> pr2 ("new file appeared:" ^ file ^ " in " ^ dir)
          | true, false -> pr2 ("old file disappeared:" ^ file  ^ " in " ^ dir)
          | true, true -> ()
        )
  )


let files_in_dirs dirs kbuild_info =
  dirs +> List.map (fun dir ->
    let dir = Common.chop_dirsymbol dir in
    (* could use assoc, but we accept "parasite" prefix *)
    let gooddirs =
      kbuild_info +> Common.map_filter (function (Directory (s, groups)) ->
        if dir =~ ("\\(.*\\)" ^ s ^ "$")
        then
          let prefix = matched1 dir in
          Some (prefix, s, groups)
        else None
      )
    in

    (match gooddirs with
    | [prefix, dir, groups] ->
        groups +> List.map (function (Group xs) ->
          Group (xs +> List.map (fun s ->
            Filename.concat (prefix ^ dir) s))
        )

    | [] ->
        pr2 ("can't find kbuild info for directory :" ^ dir);
        []
    | x::y::ys ->
        pr2 ("too much kbuild info candidate for directory :" ^ dir);
        []
    )
  ) +> List.concat




