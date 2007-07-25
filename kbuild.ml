open Common open Commonop



type kbuild_info = directory list
   and directory = Directory of string (*dirname*) * group list
   and group = Group of filename list

let directories_to_assoc xs = 
  xs +> List.map (function (Directory (s, ys)) -> s, ys)
let directories_to_hash xs = 
  xs +> directories_to_assoc +> Common.hash_of_list
let files_of_groups xs = 
  xs +> List.map (function Group ys -> ys) +> Common.union_all



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
        (function Left (d,base, "c") -> Some base | _ -> None) in
      let ys = cfiles +> List.map (fun c -> Group [c ^ ".c"]) in
      Directory (s, ys)
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
    | None, None -> raise Impossible
    | None, Some gbs -> pr2 ("new directory appeared:" ^ dir)
    | Some gas, None -> pr2 ("old directory disappeared:" ^ dir)
    | Some gas, Some gbs -> 
        let afiles = files_of_groups gas in
        let bfiles = files_of_groups gbs in
        let all_files = afiles $+$ bfiles in
        all_files +> List.iter (fun file -> 
          match List.mem file afiles, List.mem file bfiles with
          | false, false -> raise Impossible
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


          
