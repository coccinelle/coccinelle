

type diff_info =
{
    file_name: string;
    line_no: (int * int) list;
}

val getdiff : string -> diff_info list
val getpatchdiff : string -> string -> string -> diff_info list
val split : string -> string * string
