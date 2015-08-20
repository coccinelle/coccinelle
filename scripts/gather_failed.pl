#!/usr/bin/perl

# usage: gather_failed.pl **/*.failed > /tmp/big.failed

print "-*- mode: outline; -*-\n";

map {
  print "* FAILED FILE: $_\n";
  print "\n";
  system("cat $_");

} @ARGV;
