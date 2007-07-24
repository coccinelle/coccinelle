let path = Filename.concat (Sys.getenv "HOME") "coccinelle"

let std_iso = ref (Filename.concat path "standard.iso")
let std_h   = ref (Filename.concat path "standard.h")
