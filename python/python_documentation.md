# Fields and methods exposed via cocci object

## cocci.cocci_file

File name of the cocci file currently processed.

## cocci.exit()

Sets "exited" flag, that will stop the transformation of the current
file at the end of the execution of the Python script.

Example: tests/exitp.cocci

## cocci.has_env_binding(rule, name)

Returns `true` if the meta-variable `name` is bound in the rule
`rule`.

## cocci.add_pending_instance(files, virtual_rules, virtual_identifiers,
       extend_virtual_ids)

Internal function for the method iteration.register().

## cocci.make_ident(id)

Converts the string `id` to a C identifier.
(Equivalent to Coccilib.make_ident in OCaml.)

Example: tests/python_mdecl.cocci

## cocci.make_stmt(phrase)

Parses the string `phrase` as a C statement and returns the statement.

Example: tests/python_mdecl.cocci

## cocci.make_stmt_with_env(env, phrase)

Parses the string `env` as a C declaration and parses the string `phrase`
as a C statement in this environment and returns the statement.

Example: tests/python_mdecl.cocci

## cocci.make_type(type)

Parses the string `type` as a C type and returns the type.

## cocci.make_listlen(len)

Converts the integer `len` to a list length.

## cocci.make_position(fl, fn, startl, startc, endl, endc)

Returns a position with filename `fl` (a string), element `fn` (a string),
starting at line `startl`, column `startc` (integers), and
ending at line `endl`, column `endc` (integers).
(Equivalent to Coccilib.make_position in OCaml.)

Example: tests/python_mdeclp.cocci

## cocci.files()

Returns the list of current file names (as strings).
(Equivalent to Coccilib.files in OCaml.)

Example: tests/scope_id_1_python.cocci
