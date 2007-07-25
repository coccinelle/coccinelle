open Common open Commonop

module CP = Classic_patch 

(* ./split_patch ../demos/janitorings/patch-kzalloc-vnew3.patch /tmp/xx "0 -> NULL" ../bodymail.doc  *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let just_patch = ref false

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let print_header_patch patch = 
  patch +> List.iter (function CP.File (s, header, body) -> pr s)

let group_patch patch = 
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
        (Common.join "/" base, (x::yes))::aux_patch no
  in
  aux_patch patch_with_dir


let test_patch file = 
  let patch = CP.parse_patch file in
  let groups = group_patch patch in
  groups +> List.iter (fun (dir, minipatch) -> 
    print_header_patch minipatch;
    pr ""
  )

let i_to_s_padded i = 
  match i with
  | i when i < 10 -> "0" ^ i_to_s i
  | i when i < 100 -> i_to_s i
  | i -> i_to_s i

let split_patch file prefix bodymail = 
  let patch = CP.parse_patch file in
  let minipatches = group_patch patch in
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
  

  minipatches_indexed +> List.iter (fun ((dir,minipatch), i) -> 
    let numpatch = i_to_s_padded  i in
    let tmpfile = prefix ^  numpatch ^ ".mail" in
    let patchfile = "/tmp/x.patch" in
    pr2 ("generating :" ^ tmpfile ^ " for " ^ dir);

    CP.unparse_patch minipatch patchfile;


    if !just_patch
    then command2(sprintf "cat %s > %s" patchfile tmpfile)
    else begin
      Common.with_open_outfile tmpfile (fun (pr_no_nl, chan) -> 
        let pr s = pr_no_nl (s ^ "\n") in
        pr "To: linux-kernel@vger.kernel.org";
        pr (sprintf "Subject: [PATCH %s/%d] %s, for %s" 
               numpatch total subject dir);
        pr "Cc: akpm@linux-foundation.org, linux-kernel@vger.kernel.org";
        pr "BCC: padator@wanadoo.fr";
        pr "From: Yoann Padioleau <padator@wanadoo.fr>";
        pr "--text follows this line--";
        
        pr "";
        bodymail_rest +> List.iter pr;
        pr "";
        pr "Signed-off-by: Yoann Padioleau <padator@wanadoo.fr>";
        pr "";
      );

      command2(sprintf "diffstat %s >> %s" patchfile tmpfile);
      command2(sprintf "echo >> %s" tmpfile);
      command2(sprintf "cat %s >> %s" patchfile tmpfile);
    end
  )

  


(*****************************************************************************)
(* main entry point *)
(*****************************************************************************)

let main () = 
  begin
    let args = ref [] in
    let options = [
      "-just_patch", Arg.Set just_patch, "";
    ] in
    let usage_msg = 
      "Usage: " ^ basename Sys.argv.(0) ^ 
        " <patch> <prefix> <bodymailfile> [options]" ^ "\n" ^ "Options are:"
    in

    Arg.parse (Arg.align options) (fun x -> args := x::!args) usage_msg;
    args := List.rev !args;

    (match (!args) with
    | [patch] -> test_patch patch
    | [patch;prefix;bodymail] -> 
        split_patch patch prefix bodymail;
        just_patch := true;
        command2("rm -f /tmp/split_check*");
        let checkfile = "/tmp/split_check.all" in 
        let checkprefix = "/tmp/split_check-xx" in
        split_patch patch checkprefix bodymail;
        command2("cat /tmp/split_check*.mail > " ^ checkfile);
        let diff = Common.cmd_to_list (sprintf "diff %s %s " patch checkfile) 
        in
        assert(List.length diff = 0);
        
        
        
    | _ -> Arg.usage (Arg.align options) usage_msg; 
    )
  end

(*****************************************************************************)
let _ =
  main ()

