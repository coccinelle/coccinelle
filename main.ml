let start =
  Common.main_boilerplate (fun () ->
    let arglist = Array.to_list Sys.argv in
    Enter.main_with_better_error_report arglist;
    Ctlcocci_integration.print_bench();
  )
