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

if(@ARGV < 1) { die "usage: stat_directories_complete.pl  [M|C|B]";}
my $kind = "$ARGV[0]";


my $subdirs = `make subdirs`;
#my $subdirs = "rule9";
#my $subdirs = "rule1";
chomp $subdirs;
@subdirs = split /\s+/, $subdirs;


my $i = 0;
foreach my $dir (@subdirs) {
  if(-e "$dir/") {
    #print "RULE: $dir\n";

    my ($s) = 
      `cd $dir; ~/coccinelle/scripts/stat_directory_complete.pl | grep $kind:`;
    chomp $s;
    $i++;
    #print "M$i.$s\n";
    $s =~ s/$kind:/$kind$i./;
    print "$s\n";
  }

}
