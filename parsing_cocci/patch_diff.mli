

type diff_info =
{
    file_name: string;
    line_no: (int * int) list;
}

val getpatchdiff : string -> diff_info list
