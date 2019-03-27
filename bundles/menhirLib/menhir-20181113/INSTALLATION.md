# Installation

## Requirements

You need OCaml 4.02 or later, ocamlbuild, and GNU make.

## Configuration Choices

### `PREFIX`

The value of the `PREFIX` variable can be changed to control where the software,
the standard library, and the documentation are stored. These files are copied
to the following places:

```
  $PREFIX/bin/
  $PREFIX/share/menhir/
  $PREFIX/share/doc/menhir/
  $PREFIX/share/man/man1/
```

`PREFIX` must be set when invoking `make all` and `make install` (see below).

### `USE_OCAMLFIND`

The support libraries, `MenhirLib` and `MenhirSdk`, are installed either via
ocamlfind or directly in the directory `$PREFIX/share/menhir`. Installing via
ocamlfind is recommended (and is the default). It requires the `ocamlfind`
executable to be found in the `PATH`. An explicit choice can be made by setting
`USE_OCAMLFIND` to `true` or `false` when running `make all` (see below).

### `TARGET`

If your machine does not have the native code OCaml compiler (`ocamlopt`), but
does have the bytecode compiler (`ocamlc`), then you should define `TARGET=byte`
when running `make all` and `make install`.

## Compilation and Installation

Compile and install as follows:

```
       make -f Makefile PREFIX=/usr/local USE_OCAMLFIND=true all
  sudo make -f Makefile PREFIX=/usr/local install
```

If necessary, adjust `PREFIX`, `USE_OCAMLFIND` and `TARGET` as described above.
