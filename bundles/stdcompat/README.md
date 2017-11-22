``Stdcompat``: compatibility module for OCaml standard library
==============================================================

``Stdcompat`` is a compatibility layer allowing programs to use some
recent additions to the OCaml standard library while preserving the
ability to be compiled on former versions of OCaml.

The ``Stdcompat`` API is not intended to be stable, but there will be
efforts to allow future versions of ``Stdcompat`` to be compiled on a
large range of versions of OCaml: ``Stdcompat`` should compile (at least)
on every version of OCaml following 3.12.0 (included).

The module ``Stdcompat`` provides some definitions for values and
types introduced in recent versions of the standard library. These
definitions are just aliases to the matching definition of the standard
library if the latter is recent enough. Otherwise, the module
``Stdcompat`` provides an alternative implementation.

Additions to ``Pervasives`` are defined in the root of the module
``Stdcompat`` itself.  For instance, the function
``Stdcompat.really_input_string`` is an alias to
``Pervasives.really_input_string`` when the version of the OCaml
compiler is above 4.02.0, or an alternative definition otherwise.  The
types ``Stdcompat.bytes`` and ``Stdcompat.floatarray`` are aliases to
the built-in ones is the latter are available (above 4.02.0 for
``bytes`` and above 4.06.0 for ``floatarray``), and are aliases to
``string`` and ``float array`` respectively otherwise.

Sub-modules match the names of the standard library modules.  For
instance, ``Stdcompat.List.find_opt`` is an alias for
``List.find_opt`` on 4.05.0 and above, or an alternative definition
otherwise. Definitions from the standard library are reexported so that
``Stdcompat`` can be open without hiding unchanged definitions.

Functors ``Set.Make``, ``Map.Make``, ``Hashtbl.Make``, ``Weak.Make``
are redefined to provide the additional definitions appeared on recent
OCaml releases.

Note that redefinitions can have bad time complexity since the
redefinitions do not try to access to the internal representation of
the data structures when they are abstracted. For instance,
``Hashtbl.filter_map_inplace`` may have to clear and rebuild the hash
table, and ``Set.Make(Ord).find`` has a linear-time complexity.

Redefinitions cannot even guarantee some security fixes: for instance,
seeds and randomization are ignored with ``Hashtbl`` prior to 4.00.0.

See the generated documentation (in ``doc/``) for available
definitions.

``Stdcompat`` uses ``cppo`` by Martin Jambon to preprocess the source
files in order to generate the module suited for the current OCaml
compiler version. If ``cppo`` is unavailable, ``Makefile`` will
try to use ``cpp`` as fallback.