# Copyright 2012-2015, Inria
# Julia Lawall, Gilles Muller
# Copyright 2010-2011, INRIA, University of Copenhagen
# Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
# Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
# Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
# This file is part of Coccinelle.
#
# Coccinelle is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, according to version 2 of the License.
#
# Coccinelle is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
#
# The authors reserve the right to distribute this or future versions of
# Coccinelle under other licenses.


#!/usr/bin/perl

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
