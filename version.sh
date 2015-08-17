#!/bin/sh
tr -d '\n' < ./version
if test "x$MAKE_COCCI_RELEASE" = "x"; then
	./scripts/setlocalversion | tr -d '\n'
fi
