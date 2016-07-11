@initialize:python@
@@

watched_ids = ['a', 'c']

def is_watched(id):
    return id in watched_ids

@@
identifier id : script:python() { is_watched(id) };
@@

-id
+18