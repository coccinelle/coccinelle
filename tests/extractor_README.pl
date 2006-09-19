#!/usr/bin/perl

use strict;

my $ok = 0;
my $nbfiles = 0;

my $SP = "";
while(<>) {

  if(/\[status\]/) { $nbfiles++;  }
  if(/\[status\]\s*(spatch-ok|ok)\b/) { $ok++; }

  if(/Cocci\s+file\s*:\s*(\w+.cocci)/) { $SP = $1; }

}

my $pourcentcorrect = ($ok * 100.0) / $nbfiles;

print "----------------------------------------\n";
print "Total files = $nbfiles\n";
print "Correct number = $ok\n";
printf "Correct = %3.1f\%\n", $pourcentcorrect;

my $sizeSP = `cat $SP  | perl -p -e "s/\\/\\/.*//g;" | grep -v '^[ \t]*\$' | wc -l`;
chomp $sizeSP;
print "Size SP = $sizeSP\n";

my $gitinfo = `ls *.gitinfo`;
chomp $gitinfo;
print "gitinfo files = $gitinfo\n";

my $sizeP = `cat *.gitinfo | wc -l`;
chomp $sizeP;
print "Size P = $sizeP\n";

my $ratioSPvsP = ($sizeSP * 100.0) / $sizeP;
printf "Ratio SP vs P = %3.1f\%\n", $ratioSPvsP;
