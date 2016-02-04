# Copyright 2012-2014, INRIA
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


#!/usr/bin/perl -w
use strict;

sub pr2 { print "$_[0]\n"; }
sub mylog { print @_;}


# to be launched from the git directory
die "usage: $0 commithashafter [commithashbefore]" 
  if(@ARGV <= 0 || @ARGV >= 3);

# update: now I also extract the headers files, the one
# that were modified in the commit and the one that are
# locally used and that may contain useful type information
# for spatch.

# update: now I also extract some include/linux/header.h files, the
# one having the same name of one of the driver.

my $target_dir = "/tmp/extract_c_and_res/$ARGV[0]";
`mkdir -p $target_dir`;
my $old_dir = "/tmp/extract_c_and_res/$ARGV[0]_old";
`mkdir -p $old_dir`;
my $new_dir = "/tmp/extract_c_and_res/$ARGV[0]_new";
`mkdir -p $new_dir`;

my $commit_new = $ARGV[0];
my $commit_old = $ARGV[1] || "$commit_new^"; # default parent 

my $gitfile = "$target_dir/$commit_new.gitinfo";
my $makefile = "$target_dir/Makefile";

`git show $commit_new > $gitfile `;


# processing the patch 

my @files = ();
my $files = {};
my @driverheaders_in_include = ();


open FILE, "$gitfile" or die "$!";
while(<FILE>) {

  # allow other dir ? # fs|mm   there is drivers under arch/ too
  if(/^diff --git a\/((drivers|sound)\/.*?\.[ch]) b/){ 
        mylog "  $1\n";

        push @files, $1;
        $files->{$1} = 1;                
                        
    }
    elsif(/^diff --git a\/(include\/.*?\.h) b/) {
        mylog "potential header driver $1\n";
        push @driverheaders_in_include, $1;
    }
    elsif(/^diff --git a\//) {
      mylog " not driver:$_";
    }
    elsif(/^diff/) {
      die "PB: strange diff line: $_";
    }
}

# extracting the .c and .h of the patch

my $counter=0;

# to be able to later find the corresponding local included header file
my $kerneldir_of_file = {};

my @finalcfiles = ();
my $finalcfiles = {};

foreach my $f (@files) {
  my ($base) = `basename $f`;
  chomp $base;
  my $res = $base;
  if($base =~ /\.c$/) {
    $res =~ s/\.c$/.res/;
  } 
  if($base =~ /\.h$/) {
    $res =~ s/\.h$/.h.res/;
  } 

  pr2 "processing: $f $base $res";
  if(-e "$target_dir/$base") {
    $counter++;                              
    $base = "${counter}_$base";
    $res = "${counter}_$res";
    pr2 "try transform one file because already exist: $base";
    if($base =~ /\.h$/) {
      die "PB: Two header files share the same name: $base.";
    }

  }                         
  die "PB: one of the file already exist: $base" if (-e "$target_dir/$base");

  `git cat-file blob $commit_old:$f > $target_dir/$base`;
  `git cat-file blob $commit_new:$f > $target_dir/$res`;

  `git cat-file blob $commit_old:$f > $old_dir/$base`;
  `git cat-file blob $commit_new:$f > $new_dir/$base`;

  $kerneldir_of_file->{$base} = `dirname $f`;
  chomp $kerneldir_of_file->{$base};

  push @finalcfiles, $base;
  $finalcfiles->{$base} = 1;


}

# generate Makefile

open MAKE, ">$makefile" or die "$!";
print MAKE "CEDESCRIPTION=\"\"\n";
print MAKE "SP=foo.cocci\n";
print MAKE "SOURCES = ";
my $last = shift @finalcfiles;
foreach my $f (@finalcfiles) {
  print MAKE "$f \\\n\t";
}
print MAKE "$last\n";

print MAKE "

TOP=../..
include \$(TOP)/generic_makefile
";



# process potential driver headers of include/

foreach my $f (@driverheaders_in_include) {
  my $base = `basename $f`;
  chomp $base;
  if($base =~ /.h$/) {
    $base =~ s/.h$/.c/;
  } else { die "PB: internal error"; }


# julia want all .h that were in the patch, not just the headers
# of our heuristic. Hence the comment.

#  pr2 "$f $base";
#  if(defined($finalcfiles->{$base})) {
  {
#    pr2 "found header of driver in include/: $f of $base";
    my $dir = `dirname $f`;
    chomp $dir;
    `mkdir -p $target_dir/$dir`;
    `git cat-file blob $commit_old:$f > $target_dir/$f`;
    `git cat-file blob $commit_new:$f > $target_dir/$f.res`;

    `mkdir -p $old_dir/$dir`;
    `mkdir -p $new_dir/$dir`;
    `git cat-file blob $commit_old:$f > $old_dir/$f`;
    `git cat-file blob $commit_new:$f > $new_dir/$f`;
    
  }
}

# compute other linux headers not in the patch

my @linuxheaders  = `cd $target_dir; grep -E \"#include +\<[^>]*\>\" *.c *.h`;
foreach my $line (@linuxheaders) {
  chomp $line;
  #pr2 ($line);
  if($line =~ /^(.*)?:#include *\<([^>]*)\>/) {
    my ($_file, $f) = ($1, $2);

    my $base = `basename $f`;
    chomp $base;
    if($base =~ /.h$/) {
      $base =~ s/.h$/.c/;
    } else { die "PB: internal error"; }

    if(defined($finalcfiles->{$base}) && ! -e "$target_dir/include/$f") {
      pr2 "found header of driver in include/: $f of $base";
      my $dir = `dirname $f`;
      chomp $dir;
      `mkdir -p $target_dir/include/$dir`;
      `git cat-file blob $commit_old:include/$f > $target_dir/include/$f`;
    }
  } else { pr2 "pb regexp: $line"; }
}


# compute other local headers not in the patch

my @headers  = `cd $target_dir; grep -E \"#include +\\".*\\"\" *.c *.h`;

my $hfiles = {};
foreach my $line (@headers) {
  chomp $line;
  #pr2 ($line);
  if($line =~ /^(.*)?:#include *"(.*)"/) {

    my ($file, $header) = ($1, $2);
    my $dir = $kerneldir_of_file->{$file};

    my $fullheader = "$dir/$header";
    #pr2 ($fullheader);

    if($files->{$fullheader}) {
      pr2 "INFO: $fullheader was already in commit";
    } else {
      $hfiles->{$fullheader} = 1;
    }
    
  } else { pr2 "pb regexp: $line"; }
  
}

foreach my $h (keys %{$hfiles}) {
  my ($base) = `basename $h`;
  chomp $base;
  pr2 "processing additionnal header file: $h $base";

  if(-e "$target_dir/$base") {
    pr2 "-------------------------------------";
    pr2 "PB: local header (not modified in the git) $base already exists";
    pr2 "BUT I CONTINUE, but may have more .failed in the end";
    pr2 "-------------------------------------";
  } else {
    `git cat-file blob $commit_old:$h > $target_dir/$base`;
  }
}
