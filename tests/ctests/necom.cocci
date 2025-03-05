@ find_kmap_tagged_function @
comments tag : script:ocaml () { let (c1b,c1m,c1a) = List.hd tag in not (c1a = []) };
identifier fn;
@@
fn(...)@tag {
...
}

@ script:ocaml parse_kmap_tag @
fn << find_kmap_tagged_function.fn;
@@
Printf.printf "have comment in %s\n" fn
