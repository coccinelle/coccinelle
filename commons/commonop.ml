let (+>) o f = f o
let (=~) s re = Str.string_match (Str.regexp re) s 0 
let (==~) s re = Str.string_match re s 0 
