#!/bin/bash
set -e -x
cd /home/ci/coccinelle
yes | ./spatch.opt -testall
yes | ./spatch.opt -cpptestall
