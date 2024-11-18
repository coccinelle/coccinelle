#!/usr/bin/perl

# This file is part of Coccinelle, licensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website

# usage: gather_failed.pl **/*.failed > /tmp/big.failed

print "-*- mode: outline; -*-\n";

map {
  print "* FAILED FILE: $_\n";
  print "\n";
  system("cat $_");

} @ARGV;
