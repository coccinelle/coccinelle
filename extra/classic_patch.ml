open Common

type patch = patchitem list
 and patchitem = File of filename * string (* header line *) * string list

let parse_patch filename =
  let xs = Common.cat filename in
  let xxs = Common.split_list_regexp "^diff" xs in
  xxs +> List.map (fun (s, body) ->
    if s =~ "^diff --git a/\\([^ ]*\\) b/\\([^ ]*\\)"
    then begin
      let (a,b) = matched2 s in
      assert(a = b);
      File (a, s, body)
    end
    else failwith ("wrong line in git diff:" ^ s)
  )


let unparse_patch xs outfile =
  Common.with_open_outfile outfile (fun (pr_no_nl, _chan) ->
    let pr s = pr_no_nl (s ^ "\n") in

    xs +> List.iter (function (File (file, header, body)) ->
      pr header;
      body +> List.iter pr;
    )
  )
