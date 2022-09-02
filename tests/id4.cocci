@initialize:python@
@@

def make_prefix_from_type(type_name):
    # Get just the type name without surrounding whitespace or pointer asterisk.
    return type_name.strip().strip("*").strip()

def make_getter_name(prefix, member_name):
    return prefix + "_get_" + member_name

@r1@
// Getter on declared variables
type type_name;
identifier object_var;
identifier member_name;
fresh identifier getter_name = script:python(type_name, member_name) { make_getter_name(make_prefix_from_type(type_name), member_name) };
@@

type_name object_var;
<...
-object_var->member_name
+getter_name(object_var)
...>
