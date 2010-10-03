let sgrep_mode2 = ref false

let show_misc = ref true

let show_trying = ref false

let track_iso_usage = ref false

let use_glimpse = ref false

let pyoutput = ref "coccilib.output.Console"

(*"Some" value is the path with respect to which the patch should be created*)
let patch = ref (None : string option)

let make_hrule = ref (None : string (*dir*) option)

let currentfile = ref (None : string option)

let current_element = ref ""

