This directory contains bundled ocaml packages that are shipped
with coccinelle and can be used when the packages are not
installed on the build system.

By default, packages installed on the system are selected for
coccinelle's build. This directory can actually be removed if these
packages are already available.

The packages are unmodified from a particular original version:
we bundled the original tarballs and build the software from
these. In case of some packages we needed a small patch for
integration in coccinelle.

Note: the pycaml sources were taken from some repository on
git hub that provided partial python3 support. We still needed
some intricate patches to solve the non-backwards-compatibility
issues. If at some point the debian package of pycaml gets
pyton3 support too, we better switch to that version.

Note: the compilation of these libraries may be tied to
linux platforms because the Makefiles make assumptions
about file name conventions.
(dllXXX.so XXX.a etc)
