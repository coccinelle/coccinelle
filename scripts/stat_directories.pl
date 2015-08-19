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

#usage:
# cd tests-big;
# ~/coccinelle/scripts/stat_directories.pl bluetooth/* rules/* megas/*

printf "%-20s  %10s %10s %4s\n", "dir/", "failed" , "total", "%ok";
print "------------------------------------------------------\n";

$totalfailed = 0;
$total = 0;

foreach my $dir (@ARGV) {

  if(-e "$dir/") {
    my ($ok) = `find $dir -name "*.c.*ok" | wc -l`;
#    my ($ok) = `find $dir -name "*ok" | wc -l`;
    chomp $ok;
    my ($failed) = `find $dir -name "*.c.failed" | wc -l`;
#    my ($failed) = `find $dir -name "*failed" | wc -l`;
    chomp $failed;
    $totalfailed += $failed;
    my $sum = $failed + $ok;
    $total += $sum;
    if ($sum == 0) {
      print "$dir/ have 0 sum\n";
    } else {
      my $pourcent = ($ok * 100.0) / ($sum);
      printf "%-20s  %10d %10d %5.1f%%\n", "$dir/", $failed, $sum, $pourcent;
    }
  }


}

my $pourcent = (($total - $totalfailed) * 100.0) / ($total);

print "------------------------------------------------------\n";
printf "total failed = %10d / %10d % 3.1f%%\n",
  $totalfailed, $total, $pourcent;
