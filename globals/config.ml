let version = "0.1.2"

let path = 
  try (Sys.getenv "COCCINELLE_HOME") 
  with Not_found->"/usr/local/share/coccinelle"

let std_iso = ref (Filename.concat path "standard.iso")
let std_h   = ref (Filename.concat path "standard.h")
