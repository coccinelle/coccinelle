let start =
  Common.main_boilerplate (fun () ->
    let arglist = Array.to_list Sys.argv in
    Enter.main_with_better_error_report arglist;
    Ctlcocci_integration.print_bench();
    if !Enter.exit_status <> 0
    then raise (Common.UnixExit !Enter.exit_status)
  )
