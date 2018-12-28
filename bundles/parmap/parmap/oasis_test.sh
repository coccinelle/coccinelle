#!/bin/bash

set -x # DEBUG

oasis setup
ocaml setup.ml -configure -prefix `opam config var prefix`
ocaml setup.ml -build
ocaml setup.ml -reinstall
