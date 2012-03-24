#!/bin/sh -e

# build the 'configure' script
echo "running aclocal (requires 'pkg-config' macros)"
aclocal -I setup

echo "running autoconf"
autoconf
