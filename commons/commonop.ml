let (+>) o f = f o

let (==~) s re = Str.string_match re s 0 

let _match_func = ref (fun s re -> failwith "not matching regexp function")
let (=~) s re = 
  !_match_func s re
  
