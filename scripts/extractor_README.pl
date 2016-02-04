#!/usr/bin/perl

# This file is part of Coccinelle, lincensed under the terms of the GPL v2.
# See copyright.txt in the Coccinelle source code for more information.
# The Coccinelle source code can be obtained at http://coccinelle.lip6.fr

use strict;

my $ok = 0; #ok+spatch-ok
my $wrong = 0; #Error,  file level
my $fail = 0;
my $unknown = 0;
my $nbfiles = 0;

my $bugfix = 0; # site level ?
my $wrongsites = 0; # Error, site level

my $SP = "";

while(<>) {

  if(/\[status\]/) { $nbfiles++;  }

  if(/\[status\]\s*(spatch-ok|ok)\b/) { $ok++; }
  if(/\[status\]\s*(wrong)\b/) { $wrong++; }
  if(/\[status\]\s*(fail)\b/) { $fail++; }

  if(/\[status\]\s*(UNKNOWN)\b/) { $unknown++; }

  if(/Cocci\s+file\s*:\s*(\w+.cocci)/) { $SP = $1; }

}

my $pourcentcorrect = ($ok * 100.0) / $nbfiles;

print "----------------------------------------\n";
print "!!Total files = $nbfiles\n";
print "  Correct number = $ok\n";
printf "!!Correct = %3.1f\%\n", $pourcentcorrect;
print "!!Error = $wrong\n";
print "!!Bugfix (sites) = $bugfix\n";


my $sizeSP = `cat $SP  | perl -p -e "s/\\/\\/.*//g;" | grep -v '^[ \t]*\$' | wc -l`;
chomp $sizeSP;
print "!!Size SP = $sizeSP\n";

my $gitinfo = `ls *.gitinfo`;
chomp $gitinfo;
print "  gitinfo files = $gitinfo\n";

my $sizeP = `cat *.gitinfo | wc -l`;
chomp $sizeP;
print "  Size P = $sizeP\n";

my $ratioSPvsP = ($sizeSP * 100.0) / $sizeP;
printf "!!Ratio SP vs P = %3.1f\%\n", $ratioSPvsP;

my $ratioPvsSP = $sizeP / $sizeSP;
printf "!!Ratio SP vs P = %3.1f\n", $ratioPvsSP;


my $totalstatus = $ok + $fail + $wrong + $unknown;
print "----------------------------------------------------------------\n";
print "Sanity checks: nb files = $nbfiles, total status = $totalstatus\n";
print "NB UNKNOWNS = $unknown\n" if $unknown > 0;
