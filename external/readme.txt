This directory contains bundled ocaml packages that are shipped
with coccinelle and can be used when the packages are not
installed on the build system.

By default, packages installed on the system are selected for
coccinelle's build. This directory can actually be removed if these
packages are already available.


The packages are unmodifie from a particular original version, except for:
* pycaml
* sexplib


Note: the compilation of these libraries may be tied to
linux platforms because the Makefiles make assumptions
about file name convention.
(dllXXX.so XXX.a etc)
