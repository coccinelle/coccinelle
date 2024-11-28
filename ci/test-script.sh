#!/bin/bash
set -e -x
cd /home/ci/coccinelle
./spatch.opt --testall
./spatch.opt --cpptestall
