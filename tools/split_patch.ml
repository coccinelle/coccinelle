open Common open Commonop

module CP = Classic_patch 

(* ./split_patch ../demos/janitorings/patch-kzalloc-vnew3.patch /tmp/xx "0 -> NULL" ../bodymail.doc  
./split_patch /tmp/badzero.patch /tmp/xx ../mailbody.doc ../kernel_dirs.meta

update: see  http://lwn.net/Articles/284469/
for a script using git annotate to find automatically to who send
a patch (by looking at authors of lines close concerning the patch I guess
*)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let just_patch = ref false
let verbose = ref true

let pr2 s = 
  if !verbose then pr2 s

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let print_header_patch patch = 
  patch +> List.iter (function CP.File (s, header, body) -> pr s)


(*****************************************************************************)
(* Grouping strategies *)
(*****************************************************************************)

let group_patch_depth_2 patch = 
  let patch_with_dir = patch +> List.map (function (CP.File (s,header,body)) ->
    Common.split "/" (Common.dirname s), 
    (CP.File (s, header, body))
  )
  in
  let rec aux_patch xs = 
    match xs with
    | [] -> []
    | (dir_elems,x)::xs -> 
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
          then Left x
          else Right (dir_elems', x)
        ) in
        (Common.join "/" base, ["NOEMAIL"], (x::yes))::aux_patch no
  in
  aux_patch patch_with_dir



let group_patch_subsystem_info patch subinfo = 
  let patch_with_dir = patch +> List.map (function (CP.File (s,header,body)) ->
    (Common.dirname s),     (CP.File (s, header, body))
  )
  in
  let index = Maintainers.mk_inverted_index_subsystem subinfo in
  let hash = Maintainers.subsystem_to_hash subinfo in

  let rec aux_patch xs = 
    match xs with
    | [] -> []
    | ((dir,patchitem)::xs) as xs' -> 
        let leader = 
          try Hashtbl.find index dir 
          with Not_found -> failwith ("cant find leader for : " ^ dir) 
        in
        let (emailsleader, subdirs) = 
          try Hashtbl.find hash leader
          with Not_found -> failwith ("cant find subdirs of : " ^ leader) 
        in

        (match emailsleader with
        | ["NOEMAIL"] | [] | [""] -> pr2 ("no leader maintainer for: "^leader);
        | _ -> ()
        );

        let emails = ref emailsleader in
        let allsubdirs = (leader, emailsleader)::subdirs in

        let (yes, no) = xs' +> Common.partition_either (fun (dir',patchitem')->
          try (
            let emailsdir' = List.assoc dir' allsubdirs in
            emails := !emails $+$ emailsdir';
            Left patchitem'
          ) with Not_found -> Right (dir', patchitem')
         (*
          if List.mem dir' (leader::subdirs)
          then Left x
          else Right (dir', x)
         *)
        ) in
        (leader, !emails, yes)::aux_patch no
  in
  aux_patch patch_with_dir


(*****************************************************************************)
(* Split patch *)
(*****************************************************************************)
let i_to_s_padded i total = 
  match i with
  | i when i < 10 && total >= 10 -> "0" ^ i_to_s i
  | i when i < 100 -> i_to_s i
  | i -> i_to_s i

let split_patch file prefix bodymail subinfofile = 
  let patch = CP.parse_patch file in

  let subsystem_info = Maintainers.parse_subsystem_info subinfofile in
  let minipatches = group_patch_subsystem_info patch subsystem_info in
  (* let minipatches = group_patch_depth_2 patch in *)

  let total = List.length minipatches in
  let minipatches_indexed = Common.index_list_1 minipatches in

  let (subject, bodymail_rest) = 
    match Common.cat bodymail with
    | x::y::xs -> 
        if x =~ "Subject: \\(.*\\)" 
        then
          let subject = matched1 x in
          if y =~ "[-]+$" 
          then
            subject, xs
          else failwith ("wrong format for mailbody in:" ^ bodymail)
        else failwith ("wrong format for mailbody in:" ^ bodymail)
    | _ -> failwith ("wrong format for mailbody in:" ^ bodymail)
  in

  Common.command2_y_or_no ("rm -f " ^ prefix ^ "*");
  

  minipatches_indexed +> List.iter (fun ((dir,emails, minipatch), i) -> 
    let numpatch = i_to_s_padded  i total in
    let tmpfile = prefix ^  numpatch ^ ".mail" in
    let patchfile = "/tmp/x.patch" in
    pr2 ("generating :" ^ tmpfile ^ " for " ^ dir);

    CP.unparse_patch minipatch patchfile;

    let emails = 
      (match emails with
      | ["NOEMAIL"] | [] | [""] -> 
          pr2 "no maintainer"; []
      | xs -> xs
      ) @ ["akpm@linux-foundation.org"]
    in


    if !just_patch
    then command2(sprintf "cat %s > %s" patchfile tmpfile)
    else begin
      Common.with_open_outfile tmpfile (fun (pr_no_nl, chan) -> 
        let pr s = pr_no_nl (s ^ "\n") in
        pr "To: kernel-janitors@vger.kernel.org";
        pr (sprintf "Subject: [PATCH %s/%d] %s, for %s" 
               numpatch total subject dir);
        pr ("Cc: " ^ (Common.join ", " (emails @ ["linux-kernel@vger.kernel.org"])));
        pr "BCC: padator@wanadoo.fr";
        pr "From: Yoann Padioleau <padator@wanadoo.fr>";
        pr "--text follows this line--";
        
        pr "";
        bodymail_rest +> List.iter pr;
        pr "";
        pr "Signed-off-by: Yoann Padioleau <padator@wanadoo.fr>";
        emails +> List.iter (fun s -> 
          pr ("Cc: " ^ s)
        );
        pr "---";

        pr "";
      );

      command2(sprintf "diffstat -p1 %s >> %s" patchfile tmpfile);
      command2(sprintf "echo >> %s" tmpfile);
      command2(sprintf "cat %s >> %s" patchfile tmpfile);
    end
  )

  

(*****************************************************************************)
(* Test *)
(*****************************************************************************)

let test_patch file = 
  let patch = CP.parse_patch file in
  let groups = group_patch_depth_2 patch in
  groups +> List.iter (fun (dir, email, minipatch) -> 
    print_header_patch minipatch;
    pr ""
  )


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
  begin
    let args = ref [] in
    let options = [
      "-just_patch", Arg.Set just_patch, "";
      "-no_verbose", Arg.Clear verbose, "";
    ] in
    let usage_msg = 
      "Usage: " ^ basename Sys.argv.(0) ^ 
        " <patch> <prefix> <bodymailfile> <maintainerfile> [options]" ^ "\n" 
      ^ "Options are:"
    in

    Arg.parse (Arg.align options) (fun x -> args := x::!args) usage_msg;
    args := List.rev !args;

    (match (!args) with
    | [patch] -> test_patch patch
    | [patch;prefix;bodymail;subinfofile] -> 
        split_patch patch prefix bodymail subinfofile;

        command2("rm -f /tmp/split_check*");
        let checkfile = "/tmp/split_check.all" in 
        let checkprefix = "/tmp/split_check-xx" in
        save_excursion verbose (fun () -> 
        save_excursion just_patch (fun () -> 
          just_patch := true;
          verbose := false;
          split_patch patch checkprefix bodymail subinfofile;
        ));
        command2("cat /tmp/split_check*.mail > " ^ checkfile);

        let diff = Common.cmd_to_list (sprintf "diff %s %s " patch checkfile) 
        in
        let samesize = Common.filesize patch = Common.filesize checkfile in
        if (List.length diff <> 0)
        then
          if samesize 
          then pr2 "diff but at least same size"
          else pr2 "PB: diff and not same size"
        
    | _ -> Arg.usage (Arg.align options) usage_msg; 
    )
  end

(*****************************************************************************)
let _ =
  main ()

