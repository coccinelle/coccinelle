[*] marks changes that break compatibility with previous versions.

# Next version

- Fix: Add an `__iter__` method to python iterators.
  (Fixed by Laurent Mazare, https://github.com/thierry-martinez/pyml/pull/47)

- Add `Py.Seq.{of_seq_map, to_seq_map, unsafe_to_seq_map, of_list,
  of_list_map}` functions.

- Remove `Py.Import.cleanup`, which has been removed from Python 3.9, and
  was marked "for internal use only" before.
  (Reported by Victor Stinner,
   https://github.com/thierry-martinez/pyml/issues/49)

# 2020-02-22

- Fix: do not fail if GIL functions are unavailable

- Fix: include `stdcompat.h` provided with stdcompat version 13 for the
  prototype of `caml_alloc_initialized_string`.
 
- Fix: reference to the native plugin (`.cmxs`) in META

# 2020-01-15

- Compatible with OCaml 4.10.0.

- [PR 36] GC issue when registering a function with a dynamically
  allocated docstring.
  (Fixed by Laurent Mazare, https://github.com/thierry-martinez/pyml/pull/36)

- [PR 34] Ensure that every function starting with "CAMLparamK" ends with
  "CAMLreturnX".
  (Fixed by Xavier Clerc, https://github.com/thierry-martinez/pyml/pull/34)

- [GitHub issue #37] Fix test suite: 'list' object has no attribute 'clear'
  (Reported by Olaf Hering, https://github.com/thierry-martinez/pyml/issues/37)

- [PR 38] Check for executable called python3.
  (Fixed by Olaf Hering, https://github.com/thierry-martinez/pyml/pull/38)

- [PR 39] Expose is-instance and is-subclass.
  (Contribution by Laurent Mazare,
   https://github.com/thierry-martinez/pyml/pull/39)

- [PR 44] Expose some GIL functions (functions from the Python C API
  related to the global interpreter lock.
  (Contribution by Laurent Mazare,
   https://github.com/thierry-martinez/pyml/pull/44)

- Fix dynamic loading of stubs.
  (Fixed by Stéphane Glondu)

# 2019-06-26

- Support for debug build of Python library
  (Suggested by Arlen Cox:
   https://github.com/thierry-martinez/pyml/issues/18)
- Bug fix in pyml_check_symbol_available
- `Py.compile` is a wrapper for the built-in function `compile`
  (Suggested by Dhruv Makwana:
   https://github.com/thierry-martinez/pyml/issues/25)
- Guarantees for structural and physical equalities on `Py.Object.t`
  are now documented. New predicates Py.is_none, Py.is_null, Py.Bool.is_true,
  Py.Bool.is_false, Py.Tuple.is_empty.
  (Suggested by Laurent Mazare:
   https://github.com/thierry-martinez/pyml/pull/31)
- Fix Py.Array.numpy to handle OCaml GC's moving the floatarray
  (Reported by Ilias Garnier:
   https://github.com/thierry-martinez/pyml/issues/30)

# 2018-05-30

- `Py.import` is an alias for `Py.Import.import_module`.
- Use `*_opt` naming convention for the functions that return an option
  instead of an exception: `Py.import_opt`, `Py.Object.find_opt`,...
- of_seq/to_seq converters
- [*] get_attr/get_attr_string now returns option type
- Indexing operators (for OCaml 4.06.0 and above) defined in Pyops
