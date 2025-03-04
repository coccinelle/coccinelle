#!/bin/bash
set -e -x
cd /home/ci/coccinelle
./spatch.opt --ctestall
./spatch.opt --cpptestall
