[*] marks changes that break compatibility with previous versions.

Next version
============
- Support for debug build of Python library
  (Suggested by Arlen Cox:
   https://github.com/thierry-martinez/pyml/issues/18)

2018-05-30
==========

- `Py.import` is an alias for `Py.Import.import_module`.
- Use `*_opt` naming convention for the functions that return an option
  instead of an exception: `Py.import_opt`, `Py.Object.find_opt`,...
- of_seq/to_seq converters
- [*] get_attr/get_attr_string now returns option type
- Indexing operators (for OCaml 4.06.0 and above) defined in Pyops